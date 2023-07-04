import copy
import json
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from typing import TypedDict, Any, DefaultDict, Literal

from sqlalchemy import func, select

from timApp.answer.answer import Answer
from timApp.answer.answers import get_global_answers
from timApp.auth.accesshelper import (
    verify_user_create_right,
    get_doc_or_abort,
    AccessDenied,
    verify_task_access,
    verify_teacher_access,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.login import create_or_update_user
from timApp.document.docinfo import DocInfo
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, default_view_ctx
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.item import Item
from timApp.messaging.messagelist.messagelist_utils import (
    UserGroupDiff,
    sync_usergroup_messagelist_members,
)
from timApp.peerreview.util.peerreview_utils import change_peerreviewers_for_user
from timApp.plugin.importdata.importData import MissingUser, MissingUserSchema
from timApp.plugin.plugin import TaskNotFoundException, CachedPluginFinder
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.plugintype import PluginType
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.groups import do_create_group, verify_group_edit_access
from timApp.user.user import User, UserInfo, UserOrigin
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.user.userutils import grant_access, expire_access
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.utils import is_valid_email, approximate_real_name
from tim_common.marshmallow_dataclass import class_schema


class JsrunnerGroups(TypedDict, total=False):
    set: dict[str, list[int]]
    add: dict[str, list[int]]
    remove: dict[str, list[int]]


def handle_jsrunner_groups(groupdata: JsrunnerGroups | None, curr_user: User) -> None:
    if not groupdata:
        return
    groups_created = 0
    group_members_state = {}
    group_data_dict: dict[str, dict[str, list[int]]] = groupdata  # type: ignore
    for op, group_set in group_data_dict.items():
        for name, uids in group_set.items():
            ug = UserGroup.get_by_name(name)
            if not ug:
                if op == "set":
                    if groups_created >= MAX_GROUPS_PER_CALL:
                        raise RouteException(
                            f"Maximum of {MAX_GROUPS_PER_CALL} groups can be created per one jsrunner run.",
                        )
                    ug, _ = do_create_group(name)
                    groups_created += 1
                else:
                    raise RouteException(f"Group does not exist: {name}")
            else:
                verify_group_edit_access(ug, curr_user)
            if ug not in group_members_state:
                current_state = {um.user_id for um in ug.memberships_sel}
                group_members_state[ug] = UserGroupMembersState(
                    before=current_state, after=set(current_state)
                )
            users: list[User] = (
                db.session.execute(select(User).filter(User.id.in_(uids)))
                .scalars()
                .all()
            )
            found_user_ids = {u.id for u in users}
            missing_ids = set(uids) - found_user_ids
            if missing_ids:
                raise RouteException(f"Users not found: {missing_ids}")
            if op == "set":
                ug.memberships_sel = [
                    UserGroupMember(user=u, adder=curr_user) for u in users
                ]
                group_members_state[ug].after = {
                    um.user.id for um in ug.memberships_sel
                }
            elif op == "add":
                # Add by hand because memberships_sel is not updated in add_to_group
                after_set = group_members_state[ug].after
                for u in users:
                    u.add_to_group(ug, added_by=curr_user, sync_mailing_lists=False)
                    after_set.add(u.id)
            elif op == "remove":
                ug.memberships_sel = [
                    ugm
                    for ugm in ug.memberships_sel
                    if ugm.user_id not in found_user_ids
                ]
                group_members_state[ug].after = {
                    um.user.id for um in ug.memberships_sel
                }
            else:
                raise RouteException(f"Unexpected group operation: {op}")

    diffs = {
        group.id: UserGroupDiff(
            add_user_ids=list(diff.after - diff.before),
            remove_user_ids=list(diff.before - diff.after),
        )
        for group, diff in group_members_state.items()
    }

    # JSRunner group actions are permanent unlike with user UI
    sync_usergroup_messagelist_members(diffs, permanent_delete=True)


@dataclass
class NewUserInfo:
    full_name: str
    username: str
    email: str | None = None
    password: str | None = None


NewUserInfoSchema = class_schema(NewUserInfo)


@dataclass
class ItemRightActionData:
    item: str
    group: str
    action: Literal["add", "expire"]
    manageKey: str
    accessType: AccessType | None = None
    accessibleFrom: datetime | None = None
    accessibleTo: datetime | None = None


ItemRightActionSchema = class_schema(ItemRightActionData)


@dataclass
class FieldSaveResult:
    users_created: list[User] = field(default_factory=list)
    users_missing: list[UserInfo] = field(default_factory=list)
    fields_changed: int = 0
    fields_unchanged: int = 0
    fields_ignored: int = 0


class FieldSaveUserEntry(TypedDict):
    user: int
    fields: dict[str, str]


class FieldSaveRequest(TypedDict, total=False):
    savedata: list[FieldSaveUserEntry] | None
    ignoreMissing: bool | None
    allowMissing: bool | None
    createMissingUsers: bool | None
    missingUsers: Any | None
    groups: JsrunnerGroups | None
    newUsers: dict | None
    itemRightActions: dict | None


def save_fields(
    jsonresp: FieldSaveRequest,
    curr_user: User,
    current_doc: DocInfo | None = None,
    allow_non_teacher: bool = False,
    add_users_to_group: str | None = None,
    overwrite_previous_points: bool = False,
    pr_data: str | None = None,
    view_ctx: ViewContext | None = None,
) -> FieldSaveResult:
    save_obj = jsonresp.get("savedata")
    ignore_missing = jsonresp.get("ignoreMissing", False)
    allow_missing = jsonresp.get("allowMissing", False)
    ignore_fields: dict[str, bool] = {}
    groups = jsonresp.get("groups")
    view_ctx = view_ctx or default_view_ctx

    new_users_json: dict | None = jsonresp.get("newUsers")
    if new_users_json:
        new_users: list[NewUserInfo] = NewUserInfoSchema().load(
            new_users_json, many=True
        )
        if new_users:
            verify_user_create_right(curr_user)
            groups = _create_new_users(new_users, groups)

    item_right_actions_json: dict | None = jsonresp.get("itemRightActions")
    if item_right_actions_json:
        item_right_actions: list[ItemRightActionData] = ItemRightActionSchema().load(
            item_right_actions_json, many=True
        )
        if item_right_actions:
            _handle_item_right_actions(item_right_actions, curr_user)

    handle_jsrunner_groups(groups, curr_user)
    missing_users = jsonresp.get("missingUsers")
    saveresult = FieldSaveResult()
    if save_obj is None:
        save_obj = []
    if missing_users:
        m_users: list[MissingUser] = MissingUserSchema().load(missing_users, many=True)
        if jsonresp.get("createMissingUsers"):
            verify_user_create_right(curr_user)
            new_fields, users = create_missing_users(m_users)
            save_obj += new_fields
            saveresult.users_created = users
        else:
            saveresult.users_missing = [mu.user for mu in m_users]
    if not save_obj:
        return saveresult
    tasks = set()
    doc_map: dict[int, DocInfo] = {}
    user_map: dict[int, User] = {
        u.id: u
        for u in db.session.execute(
            select(User).filter(User.id.in_(x["user"] for x in save_obj))
        ).scalars()
    }

    # We need this separate "add_users_to_group" parameter because the plugin may have reported missing users.
    # They are created above, so the plugin cannot report them with "groups" in jsonresp because the user IDs are not
    # known until now.
    if add_users_to_group:
        handle_jsrunner_groups(
            {"add": {add_users_to_group: [k for k in user_map.keys()]}}, curr_user
        )

    for item in save_obj:
        task_u = item["fields"]
        for tid in task_u.keys():
            tasks.add(tid)
            try:
                id_num = TaskId.parse(
                    tid,
                    require_doc_id=False,
                    allow_block_hint=False,
                    allow_custom_field=True,
                )
            except PluginException:
                raise RouteException(f'Invalid task name: {tid.split(".")[1]}')
            if not id_num.doc_id:
                raise RouteException(f"Doc id missing: {tid}")
            if id_num.doc_id not in doc_map:
                doc_map[id_num.doc_id] = get_doc_or_abort(id_num.doc_id)
            if pr_data and pr_data in tid:
                if peer_review_data_s := task_u.get(tid):
                    user_id = item.get("user")
                    peer_review_data = json.loads(peer_review_data_s)
                    old = peer_review_data.get("from")
                    new = peer_review_data.get("to")
                    task = peer_review_data.get("task")
                    if not old:
                        # TODO: Add new reviewer or if reviewable have none
                        pass
                    if new and old and task and current_doc and user_id:
                        verify_teacher_access(current_doc)
                        change_peerreviewers_for_user(
                            current_doc, task, user_id, old, new
                        )
    task_content_name_map = {}
    for task in tasks:
        t_id = TaskId.parse(
            task, require_doc_id=True, allow_block_hint=False, allow_custom_field=True
        )
        if ignore_fields.get(t_id.doc_task, False):
            continue
        if not t_id.doc_id:
            raise RouteException(f"Doc id missing: {task}")
        dib = doc_map[t_id.doc_id]
        # TODO: Return case-specific abort messages
        if not (
            curr_user.has_teacher_access(dib)
            or (current_doc and allow_non_teacher and t_id.doc_id == current_doc.id)
            or (
                curr_user.has_view_access(dib)
                and dib.document.get_own_settings().get(
                    "allow_external_jsrunner", False
                )
            )
        ):
            raise AccessDenied(f"Missing teacher access for document {dib.id}")
        try:
            vr = verify_task_access(
                dib,
                t_id,
                AccessType.view,
                TaskIdAccess.ReadWrite,
                UserContext.from_one_user(curr_user),
                view_ctx,
            )
            plugin = vr.plugin
        except TaskNotFoundException as e:
            if not allow_missing:
                if ignore_missing:
                    ignore_fields[t_id.doc_task] = True
                    continue
                raise RouteException(str(e))
            plugin = PluginType.resolve(
                "textfield"
            )  # assuming textfield type for fields that are not in the document
        except (PluginException, TimDbException) as e:
            raise RouteException(str(e))

        # TODO this 'if' seems unnecessary
        if t_id.task_name in ("grade", "credit", "completionDate"):
            task_content_name_map[task] = "c"
            continue

        if t_id.field and t_id.field != "points" and t_id.field != "styles":
            if t_id.field == "count":
                raise RouteException("Cannot edit answer count value")
            if plugin.type == "numericfield" or plugin.type == "textfield":
                if t_id.field != plugin.get_content_field_name():  # type: ignore
                    raise RouteException(
                        f"Error saving to {task}: {t_id.field} is not an accepted field."
                    )
            task_content_name_map[task] = t_id.field
        else:
            task_content_name_map[task] = plugin.get_content_field_name()  # type: ignore

    parsed_task_ids = {
        key: TaskId.parse(
            key, require_doc_id=True, allow_block_hint=False, allow_custom_field=True
        )
        for user in save_obj
        for key in user["fields"].keys()
    }
    sq = (
        select(Answer)
        .filter(
            Answer.task_id.in_(
                [tid.doc_task for tid in parsed_task_ids.values() if not tid.is_global]
            )
            & (Answer.valid == True)
        )
        .join(User, Answer.users)
        .filter(User.id.in_(user_map.keys()))
        .group_by(User.id, Answer.task_id)
        .with_only_columns(func.max(Answer.id).label("aid"), User.id.label("uid"))
        .subquery()
    )
    datas: list[tuple[int, Answer]] = db.session.execute(
        select(Answer)
        .join(sq, Answer.id == sq.c.aid)
        .with_only_columns(sq.c.uid, Answer)
    ).all()
    global_answers = get_global_answers(parsed_task_ids)
    answer_map: defaultdict[int, dict[str, Answer]] = defaultdict(dict)
    for uid, a in datas:
        answer_map[uid][a.task_id] = a
    for uid in user_map.keys():
        for a in global_answers:
            answer_map[uid][a.task_id] = a
    cpf = CachedPluginFinder(
        doc_map=doc_map,
        curr_user=UserContext.from_one_user(curr_user),
        view_ctx=view_ctx,
    )
    for user in save_obj:
        u_id = user["user"]
        u = user_map.get(u_id)
        if not u:
            raise RouteException(f"User id {u_id} not found")
        user_fields = user["fields"]
        task_map: DefaultDict[str, dict[str, Any]] = defaultdict(dict)
        for key, value in user_fields.items():
            task_id = parsed_task_ids[key]
            if ignore_fields.get(task_id.doc_task, False):
                saveresult.fields_ignored += 1
                continue
            field = task_id.field
            if field is None:
                field = task_content_name_map[task_id.doc_task]
            task_map[task_id.doc_task][field] = value
        for taskid, contents in task_map.items():
            task_id = TaskId.parse(taskid, require_doc_id=False, allow_block_hint=False)
            if ignore_fields.get(task_id.doc_task, False):
                continue
            an: Answer | None = answer_map[u.id].get(task_id.doc_task)
            points = None
            content = {}
            new_answer = False
            points_changed = False
            if an:
                points = an.points
                content = json.loads(an.content)
            lastfield = "c"
            for c_field, c_value in contents.items():  # type: str, Any
                lastfield = c_field
                if c_field == "points":
                    if c_value == "":
                        c_value = None
                    else:
                        try:
                            c_value = float(c_value)
                        except ValueError:
                            raise RouteException(
                                f"Value {c_value} is not valid point value for task {task_id.task_name}"
                            )
                    if points != c_value:
                        points_changed = True
                    points = c_value
                elif c_field == "styles":
                    if isinstance(c_value, str):
                        try:
                            c_value = json.loads(c_value or "null")
                        except json.decoder.JSONDecodeError:
                            raise RouteException(
                                f"Value {c_value} is not valid style syntax for task {task_id.task_name}"
                            )
                    plug = cpf.find(task_id)
                    if not plug:
                        continue
                    if plug.allow_styles_field():
                        if not an or content.get(c_field) != c_value:
                            new_answer = True
                        if c_value is None:
                            content.pop(c_field, None)
                        else:
                            content[c_field] = c_value

                        # Ensure there's always a content field even when setting styles to an empty answer.
                        c_field = task_content_name_map[f"{task_id.doc_task}.{c_field}"]
                        if c_field not in content:
                            content[c_field] = None
                elif (
                    c_field == "JSSTRING"
                ):  # TODO check if this should be ALL!  No this is for settings using string
                    if not an or json.dumps(content) != c_value:
                        new_answer = True
                    content = json.loads(c_value)  # TODO: should this be inside if
                else:
                    if not an or content.get(c_field, "") != c_value:
                        new_answer = True
                    content[c_field] = c_value

            if points_changed:
                if an and not new_answer and overwrite_previous_points:
                    an.points = points
                else:
                    new_answer = True

            if not new_answer:
                saveresult.fields_unchanged += 1
                continue
            if not content:
                content[task_content_name_map[f"{task_id.doc_task}.{lastfield}"]] = None
            content_str = json.dumps(content)
            ans = Answer(
                content=content_str,
                points=points,
                task_id=task_id.doc_task,
                users=[u],
                valid=True,
                saver=curr_user,
            )
            saveresult.fields_changed += 1
            # If this was a global task, add it to all users in the answer map so we won't save it multiple times.
            if task_id.is_global:
                for uid in user_map.keys():
                    answer_map[uid][ans.task_id] = ans
            db.session.add(ans)
    return saveresult


def _create_new_users(
    users: list[NewUserInfo], groups: JsrunnerGroups | None
) -> JsrunnerGroups | None:
    user_infos = [
        UserInfo(
            full_name=u.full_name,
            email=u.email,
            username=u.username,
            password=u.password,
            origin=UserOrigin.JSRunner,
        )
        for u in users
    ]
    created_users = [_create_user_from_info(ui) for ui in user_infos]
    db.session.flush()
    if not groups:
        return None

    # Rewrite group IDs to correct user IDs in case they are also being added to a group
    groups_dict: dict[str, dict[str, list[int]]] = copy.deepcopy(groups)  # type: ignore
    for group_info in groups_dict.values():
        for uids in group_info.values():
            for i, uid in enumerate(uids):
                if uid < 0:
                    cu = created_users[-uid - 1]
                    if cu:
                        uids[i] = cu.id
    return groups_dict  # type: ignore


class UserFieldEntry(TypedDict):
    user: int
    fields: dict[str, str]


def create_missing_users(
    users: list[MissingUser],
) -> tuple[list[UserFieldEntry], list[User]]:
    created_users = []
    for mu in users:
        u = _create_user_from_info(mu.user)
        if u:
            created_users.append(u)
    db.session.flush()
    fields = []
    for u, missing_u in zip(created_users, users):
        fields.append(
            UserFieldEntry(
                user=u.id,
                fields=missing_u.fields,
            )
        )
    return fields, created_users


MAX_GROUPS_PER_CALL = 10


@dataclass
class UserGroupMembersState:
    before: set[int]
    after: set[int]


def _create_user_from_info(ui: UserInfo) -> User | None:
    if ui.username is None and ui.email is None:
        return None
    if ui.email is not None:
        # A+ may give users with invalid mails like '6128@localhost'. Just skip over those.
        if ui.email.endswith("@localhost"):
            return None
        if not is_valid_email(ui.email):
            raise RouteException(f'Invalid email: "{ui.email}"')
    if ui.username is None:
        ui.username = ui.email
    if ui.full_name is None and ui.email is not None:
        # Approximate real name with the help of email.
        # This won't be fully accurate, but we can't do better.
        ui.full_name = approximate_real_name(ui.email)

    user = None
    if ui.username:
        user = User.get_by_name(ui.username)
    if not user and ui.email:
        user = User.get_by_email(ui.email)

    # If the user wasn't created by JSRunner, don't overwrite it
    if user and user.origin != UserOrigin.JSRunner:
        return user

    # In any case, do not allow updating the email or username
    return create_or_update_user(ui, update_username=False, update_email=False)


def _handle_item_right_actions(
    item_right_actions: list[ItemRightActionData], curr_user: User
) -> None:
    # Group actions by item ID, also check for permission
    item_actions: dict[int, list[ItemRightActionData]] = {}
    items: dict[int, Block] = {}

    for action in item_right_actions:
        # Items can be declared either as path or ID, so it's easier to find_by_path for everything
        item = Item.find_by_path(action.item, fallback_to_id=True)
        if not item:
            raise NotExist()

        # TODO: Allow changing folder rights when the plugins can be "signed"
        #   (currently, it's dangerous to just verify permissions)
        if isinstance(item, Folder):
            raise RouteException("Managing folder rights is not yet supported")

        if item.block.id not in item_actions:
            if isinstance(item, DocInfo):
                settings = item.document.get_settings()
                if settings.manage_key() != action.manageKey:
                    raise RouteException(
                        f"Invalid manage key for document '{item.path}'"
                    )

            item_actions[item.block.id] = []
            items[item.block.id] = item.block

        item_actions[item.block.id].append(action)

    # Apply actions as we go
    for item_id, actions in item_actions.items():
        item = items[item_id]
        for action in actions:
            group = UserGroup.get_by_name(action.group)
            access_type = action.accessType or AccessType.view
            if not group:
                raise NotExist(f"Group '{action.group}' does not exist")
            match action.action:
                case "add":
                    grant_access(
                        group,
                        item,
                        access_type,
                        accessible_from=action.accessibleFrom,
                        accessible_to=action.accessibleTo,
                    )
                case "expire":
                    expire_access(group, item, access_type)

    # Flush so that access rights are correct for any other JSRunner operations
    db.session.flush()
