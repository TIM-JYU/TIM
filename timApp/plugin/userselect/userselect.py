from dataclasses import dataclass, field
from datetime import timedelta
from enum import Enum

import filelock
from flask import render_template_string, Response, current_app
from sqlalchemy import select

from timApp.answer.backup import sync_user_group_memberships_if_enabled
from timApp.auth.accesshelper import verify_logged_in, verify_view_access, verify_admin
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.session.util import distribute_session_verification
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.editing.globalparid import GlobalParId
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewRoute, ViewContext
from timApp.item.manage import (
    TimeOpt,
    verify_permission_edit_access,
    PermissionEditModel,
    add_perm,
    log_right,
    remove_perm,
)
from timApp.plugin.jsrunner.util import (
    FieldSaveUserEntry,
    FieldSaveRequest,
    save_fields,
)
from timApp.plugin.plugin import Plugin
from timApp.plugin.userselect.action_queue import (
    register_group_add_action,
    register_group_remove_action,
    register_dist_right_action,
    apply_pending_actions_impl,
)
from timApp.plugin.userselect.dist_right_util import (
    DistributeRightAction,
    apply_dist_right_actions,
)
from timApp.plugin.userselect.utils import group_expired_offset
from timApp.timdb.sqa import db, run_sql
from timApp.user.groups import verify_group_edit_access
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import membership_current, UserGroupMember
from timApp.user.userutils import ReplaceAccessAction
from timApp.util.flask.requesthelper import (
    NotExist,
    RouteException,
    view_ctx_with_urlmacros,
)
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.get_fields import get_fields_and_users, RequestedGroups
from timApp.util.logger import log_warning, log_info
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
    EditorTab,
)
from tim_common.utils import DurationSchema

user_select_plugin = TypedBlueprint("userSelect", __name__, url_prefix="/userSelect")


@dataclass
class UserSelectInputModel:
    pass


@dataclass
class PermissionActionBase:
    doc_path: str
    type: AccessType


@dataclass
class AddPermission(PermissionActionBase):
    time: TimeOpt
    confirm: bool = False
    replaceAction: ReplaceAccessAction = field(
        default=ReplaceAccessAction.UpdateDuration, metadata={"by_value": True}
    )


@dataclass
class RemovePermission(PermissionActionBase):
    pass


@dataclass
class ConfirmPermission(PermissionActionBase):
    pass


@dataclass
class ChangePermissionTime(PermissionActionBase):
    minutes: float


@dataclass
class SetTaskValueAction:
    taskId: str
    value: str


@dataclass
class ChangeGroupAction:
    changeTo: str
    allGroups: list[str]
    verify: bool = False

    @property
    def all_groups(self) -> list[str]:
        return self.allGroups + [self.changeTo]


@dataclass
class ActionCollection:
    addPermission: list[AddPermission] = field(default_factory=list)
    confirmPermission: list[ConfirmPermission] = field(default_factory=list)
    removePermission: list[RemovePermission] = field(default_factory=list)
    changePermissionTime: list[ChangePermissionTime] = field(default_factory=list)
    distributeRight: list[DistributeRightAction] = field(default_factory=list)
    setValue: list[SetTaskValueAction] = field(default_factory=list)
    addToGroups: list[str] = field(default_factory=list)
    removeFromGroups: list[str] = field(default_factory=list)
    changeGroup: ChangeGroupAction | None = None
    verifyRemoteSessions: list[str] = field(default_factory=list)
    invalidateRemoteSessions: list[str] = field(default_factory=list)


@dataclass
class ScannerOptions:
    applyOnMatch: bool = False
    continuousMatch: bool = False
    enabled: bool = False
    scanInterval: float = 1.5
    waitBetweenScans: float = 0.0
    beepOnSuccess: bool = False
    beepOnFailure: bool = False
    parameterSeparator: str | None = "#"


@dataclass
class TextOptions:
    apply: str | None = None
    cancel: str | None = None
    success: str | None = None
    undone: str | None = None
    undo: str | None = None
    undoWarning: str | None = None
    verifyReasons: dict[str, str] = field(default_factory=dict)


@dataclass
class UserSelectMarkupModel(GenericMarkupModel):
    allowUndo: bool = False
    preFetch: bool = False
    inputMinLength: int = 3
    autoSearchDelay: float = 0.0
    selectOnce: bool = False
    maxMatches: int = 10
    useActionQueues: bool = False
    synchronizedGroupActions: bool = True
    scanner: ScannerOptions = field(default_factory=ScannerOptions)
    groups: list[str] = field(default_factory=list)
    fields: list[str] = field(default_factory=list)
    actions: ActionCollection | None = None
    text: TextOptions = field(default_factory=TextOptions)
    displayFields: list[str] = field(default_factory=lambda: ["username", "realname"])
    sortBy: list[str] = field(default_factory=list)
    verifyActionsField: str | None = None


UserSelectMarkupModelSchema = class_schema(
    UserSelectMarkupModel, base_schema=DurationSchema
)


@dataclass
class UserSelectStateModel:
    pass


@dataclass
class UserSelectHtmlModel(
    GenericHtmlModel[UserSelectInputModel, UserSelectMarkupModel, UserSelectStateModel]
):
    def get_component_html_name(self) -> str:
        return "user-selector"

    def get_static_html(self) -> str:
        return render_template_string(
            """
                <div>User selector</div>
            """
        )


def reqs_handler() -> PluginReqs:
    template = """
``` {#user_select plugin="userSelect" nocache="true"}
groups:                  # Groups from which the users can be searched and selected
    - groupname
fields:                  # Fields to use when searching. Username, full name and email are always included.
    - somefields
displayFields:           # What fields to show in search results. Any fields defined in "fields" are allowed.
  - realname              # Full name
  - username              # Username
  - useremail             # Email address
autoSearchDelay: 0       # Wait this amount of seconds before searching for a user. 0 = OFF
maxMatches: 10           # If more than one user is found, how many are shown at max.
inputMinLength: 3        # How many characters must be given before searching.
selectOnce: false        # If true, hide other users when selecting one.
allowUndo: false         # Can the action be undone. Undoing is not supported by all actions.
preFetch: false          # If true, all users are prefetched. This makes initial load longer but searches are faster.
verifyActionsField: null # Name of the verify field. If specified, userSelect will show a verification message with the value of this field before applying an action.
scanner:                 # Camera scanner options
  enabled: false           # Show the scanner button
  parameterSeparator: "#"  # String to separate the user query from the search parameter when scanning. If null, no separation is done.
  applyOnMatch: false      # If true and only one user is found from scanning, apply actions on them without verifying 
  continuousMatch: false   # If true, scanner is not disabled after a successful scan
  waitBetweenScans: 0      # If continuousMatch is true, how many seconds to wait before restarting the scanner
  beepOnSuccess: false     # Play a "beep" sound on successful scan.
  beepOnFailure: true      # Play a "beep" sound if scan was successful but no matching users were found. 
  scanInterval: 1.5        # Time interval between scan attempts in seconds. Lower value means faster scans but higher energy usage.
actions:                 # Actions to apply for the selected user

    #addPermission:               # Add permissions to documents
    #  - doc_path: some/doc/path  # Target document path
    #    type: view               # Permission type. Allowed values: view, edit, teacher, manage, see_answers, owner, copy
    #    time:                    # Duration of the permission
    #      type: always                            # Permission type. Allowed values: always, range, duration
    #      duration: P30M                         # Duration in ISO 8601 format (for "duration" type)
    #      to: 2021-04-23T18:00:00.000Z           # When the permission ends
    #      from: 2021-04-23T16:00:00.000Z         # When the permission starts
    #      durationTo: 2021-04-23T18:00:00.000Z   # When the duration permission can be asked for
    #      durationFrom: 2021-04-23T16:00:00.000Z # When the duration permission ends
    #    confirm: false          # Does the permission require extra confirmation? 

    #confirmPermission:           # Confirms user's permission to a document
    #  - doc_path: some/doc/path  # Target document path
    #    type: view               # Permission type. Allowed values: view, edit, teacher, manage, see_answers, owner, copy

    #removePermission:              # Remove permissions from documents
    #  - doc_path: some/doc/path    # Document from which to remove the permission
    #    type: view                 # Type of permission to remove: view, edit, teacher, manage, see_answers, owner, copy

    #changePermissionTime:          # Change duration of an active user permission
    #  - doc_path: some/doc/path    # Document from which to edit the permission time
    #    type: view                 # Type of permission to change: view, edit, teacher, manage, see_answers, owner, copy
    #    minutes: 10                # By how many minutes to adjust the permission. Positive values add time, negative remove.

    #setValue:                   # Set value to a field or task
    #  - taskId: 1.sometask         # Task or field to which to set the value
    #    value: somevalue           # Value to set. Can be a macro.

    #addToGroups:               # Add the user to the groups
    #  - somegroups

    #changeGroup:                 # Change the user's group
    #  changeTo: somegroup        # Group to change to. The user will become a member of this group.
    #  allGroups:                 # All groups to check. User is removed from other all groups that are not the same as changeTo.
    #    - somegroup
    #  verify: false              # Show a verification message if user is already a member of a group in allGroups.

    #removeFromGroups:          # Remove the user from the groups. This will do a soft delete (i.e. add removal date)
    #  - somegroups

    #verifyRemoteSessions:    # Verify sessions for remote targets. Use DIST_RIGHTS_HOSTS to specify the actual hosts.
    #  - target1
    
    #invalidateRemoteSessions: # Invalidate sessions for remote targets. Use DIST_RIGHTS_HOSTS to specify the actual hosts.
    #  - target1
#text:              # UI texts
#  apply: Apply permissions
#  cancel: Cancel
#  success: Gave permission to {realname}.
#  undone: Undone permissions from {realname} ({HETU}).
```
"""
    editor_tabs: list[EditorTab] = [
        {
            "text": "Plugins",
            "items": [
                {
                    "text": "UserSelect",
                    "items": [
                        {
                            "data": template.strip(),
                            "text": "User selector",
                            "expl": "Search users from a group and apply actions to them",
                        }
                    ],
                },
            ],
        },
    ]
    return {
        "js": ["userSelect"],
        "multihtml": True,
        "editor_tabs": editor_tabs,
    }


def log_user_select(msg: str) -> None:
    if not current_app.config["LOG_USER_SELECT_ACTIONS"]:
        return
    log_info(f"USER_SELECT: {msg}")


def get_plugin_markup(
    task_id: str | None, par: GlobalParId | None
) -> tuple[UserSelectMarkupModel, DocInfo, User, ViewContext]:
    verify_logged_in()
    user = get_current_user_object()
    user_ctx = UserContext.from_one_user(user)
    view_ctx = view_ctx_with_urlmacros(ViewRoute.Unknown)

    if task_id:
        plugin, doc = Plugin.from_task_id(task_id, user_ctx, view_ctx)
    elif par:
        plugin, doc = Plugin.from_global_par(par, user_ctx, view_ctx)
    else:
        raise RouteException("Either task_id or par must be specified")
    model: UserSelectMarkupModel = UserSelectMarkupModelSchema().load(plugin.values)
    return model, doc, user, view_ctx


@user_select_plugin.get("/fetchUsers")
def fetch_users(
    task_id: str | None = None,
    doc_id: int | None = None,
    par_id: str | None = None,
) -> Response:
    model, doc, user, view_ctx = get_plugin_markup(
        task_id, GlobalParId(doc_id, par_id) if doc_id and par_id else None
    )
    field_data, _, field_names, _ = get_fields_and_users(
        model.fields, RequestedGroups.from_name_list(model.groups), doc, user, view_ctx
    )
    return json_response(
        {
            "users": [
                {"user": field_obj["user"], "fields": field_obj["fields"]}
                for field_obj in field_data
            ],
            "fieldNames": field_names,
        }
    )


def match_query(query_words: list[str], keywords: list[str]) -> bool:
    kw = set(keywords)
    for qw in query_words:
        found = next((k for k in kw if qw in k), None)
        if found is None:
            return False
        kw.remove(found)
    return True


@user_select_plugin.post("/search")
def search_users(
    search_strings: list[str],
    task_id: str | None = None,
    par: GlobalParId | None = None,
) -> Response:
    model, doc, user, view_ctx = get_plugin_markup(task_id, par)
    verify_view_access(doc)
    field_data, _, field_names, _ = get_fields_and_users(
        model.fields, RequestedGroups.from_name_list(model.groups), doc, user, view_ctx
    )

    # If query contains spaces, split into sub-queries that all must match
    # In each subquery, match by longest word first to ensure best match
    search_query_words = [
        sorted(s.lower().split(), key=lambda s: len(s), reverse=True)
        for s in search_strings
    ]
    matched_field_data = []
    for field_obj in field_data:
        fields = field_obj["fields"]
        usr = field_obj["user"]
        values_to_check: list[str | float | None | None] = [
            usr.name,
            usr.real_name,
            usr.email,
            *fields.values(),
        ]

        for field_val in values_to_check:
            if not field_val:
                continue
            val = (
                (field_val if isinstance(field_val, str) else str(field_val))
                .lower()
                .split()
            )
            if next((qws for qws in search_query_words if match_query(qws, val)), None):
                matched_field_data.append(field_obj)
                break

    match_count = len(matched_field_data)
    if match_count > model.maxMatches:
        matched_field_data = matched_field_data[0 : model.maxMatches]

    return json_response(
        {
            "matches": [
                {"user": field_obj["user"], "fields": field_obj["fields"]}
                for field_obj in matched_field_data
            ],
            "allMatchCount": match_count,
            "fieldNames": field_names,
        }
    )


def has_distribution_moderation_access(doc: DocInfo) -> bool:
    allowed_docs = current_app.config.get("DIST_RIGHTS_MODERATION_DOCS", [])
    return doc.path in allowed_docs


def get_plugin_info(
    username: str, task_id: str | None = None, par: GlobalParId | None = None
) -> tuple[UserSelectMarkupModel, User, UserGroup, User, DocInfo]:
    model, doc, _, _ = get_plugin_markup(task_id, par)
    # Ensure user actually has access to document with the plugin
    verify_view_access(doc)

    cur_user = get_current_user_object()
    user_group = UserGroup.get_by_name(username)
    user_acc = User.get_by_name(user_group.name)

    assert user_acc is not None
    if not user_group:
        raise RouteException(f"Cannot find user {username}")

    if not model.actions:
        return model, cur_user, user_group, user_acc, doc

    can_distribute_rights = has_distribution_moderation_access(doc)

    if model.actions.distributeRight and not can_distribute_rights:
        raise RouteException("distributeRight is not allowed in this document")

    if model.actions.verifyRemoteSessions and not can_distribute_rights:
        raise RouteException("verifyRemoteSessions is not allowed in this document")

    if model.actions.invalidateRemoteSessions and not can_distribute_rights:
        raise RouteException("invalidateRemoteSessions is not allowed in this document")

    return model, cur_user, user_group, user_acc, doc


def undo_dist_right_actions(user_acc: User, model: UserSelectMarkupModel) -> list[str]:
    if not model.actions:
        return []
    # TODO: Implement undoing for local permissions
    undoable_dists = [
        dist
        for dist in model.actions.distributeRight
        if dist.operation in ("confirm", "quit")
    ]
    undo_actions = []
    for distribute in undoable_dists:
        if distribute.operation == "confirm":
            undo_action = DistributeRightAction(
                operation="undoconfirm",
                target=distribute.target,
                timestamp=distribute.timestamp_or_now,
            )
        elif distribute.operation == "quit":
            undo_action = DistributeRightAction(
                operation="undoquit",
                target=distribute.target,
                timestamp=distribute.timestamp_or_now,
            )
        elif distribute.operation == "changetime":
            undo_action = DistributeRightAction(
                operation="changetime",
                target=distribute.target,
                timestamp=distribute.timestamp_or_now,
                minutes=-distribute.minutes,
            )
        else:
            continue

        undo_actions.append(undo_action)

    return apply_dist_actions(user_acc, model, undo_actions)


def undo_field_actions(
    cur_user: User, user_acc: User, set_value: list[SetTaskValueAction]
) -> None:
    fields_to_save = {set_val.taskId: "" for set_val in set_value}
    if fields_to_save:
        # Reuse existing helper for answer route to save field values quickly
        save_fields(
            FieldSaveRequest(
                savedata=[FieldSaveUserEntry(user=user_acc.id, fields=fields_to_save)]
            ),
            cur_user,
            allow_non_teacher=False,
        )


def get_groups(
    cur_user: User, add: list[str], remove: list[str], change_all_groups: list[str]
) -> tuple[list[UserGroup], list[UserGroup], list[UserGroup]]:
    add_groups: list[UserGroup] = list(
        run_sql(select(UserGroup).filter(UserGroup.name.in_(add))).scalars().all()
    )
    remove_groups: list[UserGroup] = list(
        run_sql(select(UserGroup).filter(UserGroup.name.in_(remove))).scalars().all()
    )
    change_all_groups_ugs: list[UserGroup] = list(
        run_sql(select(UserGroup).filter(UserGroup.name.in_(change_all_groups)))
        .scalars()
        .all()
    )
    all_groups: dict[str, UserGroup] = {
        ug.name: ug for ug in (add_groups + remove_groups + change_all_groups_ugs)
    }

    for ug in all_groups.values():
        if ug.is_sisu:
            raise RouteException(
                "Modifying Sisu groups with user selector is not allowed to prevent mistakes"
            )
        verify_group_edit_access(ug, cur_user)

    return add_groups, remove_groups, change_all_groups_ugs


def undo_group_actions_locked(
    user_acc: User,
    cur_user: User,
    add: list[str],
    remove: list[str],
    change: ChangeGroupAction | None,
) -> bool:
    add_groups, remove_groups, change_all_groups = get_groups(
        cur_user, add, remove, change.all_groups if change else []
    )

    # We cannot safely add user back to a group because we don't know if the user was removed from it
    changed = False
    for ug in add_groups + change_all_groups:
        membership = ug.current_memberships.get(user_acc.id, None)
        if membership:
            changed = True
            membership.set_expired(time_offset=group_expired_offset)

    return changed


def undo_group_actions_queued(
    user_acc: User,
    cur_user: User,
    add: list[str],
    remove: list[str],
    change: ChangeGroupAction | None,
) -> None:
    add_groups, remove_groups, change_all_groups = get_groups(
        cur_user, add, remove, change.all_groups if change else []
    )

    for ug in add_groups + change_all_groups:
        register_group_remove_action(user_acc, ug.name)


def undo_group_actions(
    user_acc: User,
    cur_user: User,
    model: UserSelectMarkupModel,
) -> None:
    if not model.actions:
        return
    if model.useActionQueues:
        undo_group_actions_queued(
            user_acc,
            cur_user,
            model.actions.addToGroups,
            model.actions.removeFromGroups,
            model.actions.changeGroup,
        )
    else:
        changed = undo_group_actions_locked(
            user_acc,
            cur_user,
            model.actions.addToGroups,
            model.actions.removeFromGroups,
            model.actions.changeGroup,
        )
        # Flush so that right distribution is handled correctly
        db.session.flush()

        if changed:
            sync_user_group_memberships_if_enabled(user_acc)


@user_select_plugin.post("/undo")
def undo(
    username: str,
    task_id: str | None = None,
    par: GlobalParId | None = None,
    param: str | None = None,  # TODO: Use
) -> Response:
    model, cur_user, user_group, user_acc, doc = get_plugin_info(username, task_id, par)
    # No permissions to undo
    if not model.actions:
        return json_response({"distributionErrors": []})

    log_user_select(
        f"[{cur_user.name}] undo on {user_acc.name} in {doc.path} (param = {param})"
    )

    locks = get_group_action_locks(model)
    for lock in locks:
        lock.acquire()

    try:
        # Undo the group actions before dist rights because dist rights might depend on the group
        # Moreover, undoing is generally a soft action, so we can always manually restore the group safely
        undo_group_actions(user_acc, cur_user, model)

        errors = undo_dist_right_actions(user_acc, model)

        # If there are errors undoing, don't reset the fields because it may have been caused by a race condition
        if errors:
            db.session.rollback()
            return json_response({"distributionErrors": errors})

        undo_field_actions(cur_user, user_acc, model.actions.setValue)

        errors += apply_verify_session(
            "verify", user_acc, param, model.actions.invalidateRemoteSessions
        )

        errors += apply_verify_session(
            "invalidate", user_acc, param, model.actions.verifyRemoteSessions
        )

        db.session.commit()
    finally:
        for lock in locks:
            lock.release()

    return json_response({"distributionErrors": errors})


@user_select_plugin.get("/applyPendingActions")
def apply_pending_actions_route() -> Response:
    verify_admin()
    apply_pending_actions_impl()
    return ok_response()


def apply_permission_actions(
    user_group: UserGroup,
    add: list[AddPermission],
    remove: list[RemovePermission],
    confirm: list[ConfirmPermission],
    change_time: list[ChangePermissionTime],
) -> list[str]:
    doc_entries = {}
    update_messages = []

    permission_actions: list[PermissionActionBase] = [
        *add,
        *remove,
        *confirm,
        *change_time,
    ]

    # Verify first that all documents can be accessed and permissions edited + cache doc entries
    for perm in permission_actions:
        if perm.doc_path in doc_entries:
            continue
        doc_entry = DocEntry.find_by_path(
            perm.doc_path, fallback_to_id=True, try_translation=False
        )
        if not doc_entry:
            raise NotExist(f"Can't find document {perm.doc_path}")
        verify_permission_edit_access(doc_entry, perm.type)
        doc_entries[perm.doc_path] = doc_entry

    for to_add in add:
        doc_entry = doc_entries[to_add.doc_path]
        # Don't throw if we try to remove a permission from ourselves, just ignore it
        # noinspection PyTypeChecker
        accs = add_perm(
            PermissionEditModel(
                to_add.type, to_add.time, [user_group.name], to_add.confirm, True
            ),
            doc_entry,
            replace_active_duration=to_add.replaceAction,
        )
        if accs:
            update_messages.append(
                f"added {accs[0].info_str} for {user_group.name} in {doc_entry.path}"
            )

    for to_remove in remove:
        doc_entry = doc_entries[to_remove.doc_path]
        a = remove_perm(user_group, doc_entry.block, to_remove.type)
        # A document's translations and velp groups will have their permissions removed by default as well,
        # so we need to iterate the list returned from remove_perm
        for p in a:
            update_messages.append(
                f"removed {p.info_str} for {user_group.name} in {doc_entry.path}"
            )

    for to_confirm in confirm:
        doc_entry = doc_entries[to_confirm.doc_path]
        ba_confirm: BlockAccess | None = (
            run_sql(
                select(BlockAccess)
                .filter_by(
                    type=to_confirm.type.value,
                    block_id=doc_entry.block.id,
                    usergroup_id=user_group.id,
                )
                .limit(1)
            )
            .scalars()
            .first()
        )
        if ba_confirm and ba_confirm.require_confirm:
            ba_confirm.do_confirm()
            update_messages.append(
                f"confirmed {ba_confirm.info_str} for {user_group.name} in {doc_entry.path}"
            )

    for to_change in change_time:
        doc_entry = doc_entries[to_change.doc_path]
        ba_change: BlockAccess | None = (
            run_sql(
                select(BlockAccess)
                .filter_by(
                    type=to_change.type.value,
                    block_id=doc_entry.block.id,
                    usergroup_id=user_group.id,
                )
                .limit(1)
            )
            .scalars()
            .first()
        )
        if ba_change and ba_change.accessible_to is not None:
            ba_change.accessible_to += timedelta(minutes=to_change.minutes)
            update_messages.append(
                f"adjusted {ba_change.info_str} for {user_group.name} in {doc_entry.path} by {to_change.minutes} minutes"
            )

    return update_messages


def apply_field_actions(
    cur_user: User, user_acc: User, set_values: list[SetTaskValueAction]
) -> None:
    fields_to_save = {set_val.taskId: set_val.value for set_val in set_values}
    if fields_to_save:
        # Reuse existing helper for answer route to save field values quickly
        save_fields(
            FieldSaveRequest(
                savedata=[FieldSaveUserEntry(user=user_acc.id, fields=fields_to_save)]
            ),
            cur_user,
            allow_non_teacher=False,
        )


def apply_verify_session(
    action: str, user_acc: User, session_id: str | None, targets: list[str]
) -> list[str]:
    return distribute_session_verification(action, user_acc.name, session_id, targets)


def apply_group_actions_locked(
    user_acc: User,
    cur_user: User,
    add: list[str],
    remove: list[str],
    change_to: ChangeGroupAction | None,
) -> bool:
    add_groups, remove_groups, _ = get_groups(cur_user, add, remove, [])
    changed = False

    for ug in add_groups:
        user_acc.add_to_group(ug, cur_user)
        changed = True

    def expire_membership(ugg: UserGroup) -> None:
        nonlocal changed
        m = user_acc.memberships_dyn.filter(
            membership_current & (UserGroupMember.group == ugg)
        ).first()
        if m:
            m.set_expired(time_offset=group_expired_offset)
            changed = True

    for ug in remove_groups:
        expire_membership(ug)

    if change_to:
        all_groups_set = set(change_to.allGroups)
        change_to_group = UserGroup.get_by_name(change_to.changeTo)
        if change_to_group:
            user_acc.add_to_group(change_to_group, cur_user)
            changed = True
        for ug in user_acc.groups:
            if ug.name != change_to.changeTo and ug.name in all_groups_set:
                expire_membership(ug)

    return changed


def apply_group_actions_queued(
    user_acc: User,
    cur_user: User,
    add: list[str],
    remove: list[str],
    change_to: ChangeGroupAction | None,
) -> None:
    add_groups, remove_groups, change_to_group = get_groups(
        cur_user, add, remove, change_to.allGroups if change_to else []
    )

    for a in add_groups:
        register_group_add_action(user_acc, a.name)

    for r in remove_groups:
        register_group_remove_action(user_acc, r.name)

    if change_to:
        register_group_add_action(user_acc, change_to.changeTo)
        for g in change_to_group:
            if g.name != change_to.changeTo:
                register_group_remove_action(user_acc, g.name)


class NeedsVerifyReasons(Enum):
    CHANGE_GROUP_BELONGS = "changeGroupBelongs"
    CHANGE_GROUP_ALREADY_MEMBER = "changeGroupAlreadyMember"


@user_select_plugin.post("/needsVerify")
def needs_verify(username: str, par: GlobalParId) -> Response:
    model, cur_user, user_group, user_acc, doc = get_plugin_info(username, None, par)

    if not model.actions or not model.verifyActionsField:
        return json_response({"needsVerify": False, "reasons": []})

    verify_reasons: list[NeedsVerifyReasons | str] = []

    if model.actions.changeGroup and model.actions.changeGroup.verify:
        membership_groups: set[str] = {g.name for g in user_acc.groups}
        if model.actions.changeGroup.changeTo in membership_groups:
            verify_reasons.append(NeedsVerifyReasons.CHANGE_GROUP_ALREADY_MEMBER)
        # No need to add the other reason because "already member" is more important
        elif any(
            True for g in model.actions.changeGroup.allGroups if g in membership_groups
        ):
            verify_reasons.append(NeedsVerifyReasons.CHANGE_GROUP_BELONGS)

    if model.verifyActionsField:
        view_ctx = view_ctx_with_urlmacros(ViewRoute.Unknown)
        field_data, _, _, _ = get_fields_and_users(
            [model.verifyActionsField],
            RequestedGroups([user_group]),
            doc,
            cur_user,
            view_ctx,
        )

        for r in field_data:
            fields = r["fields"]
            for val in fields.values():
                verify_reasons.append(str(val))

    return json_response(
        {"needsVerify": len(verify_reasons) > 0, "reasons": verify_reasons}
    )


def get_group_action_locks(
    model: UserSelectMarkupModel,
) -> list[filelock.BaseFileLock]:
    if not model.synchronizedGroupActions:
        return []
    if model.useActionQueues:
        return []
    if not model.actions:
        return []
    all_groups = set(model.actions.addToGroups) | set(model.actions.removeFromGroups)
    if model.actions.changeGroup:
        all_groups |= set(model.actions.changeGroup.all_groups)
    locks: list[filelock.BaseFileLock] = [
        filelock.FileLock(f"/tmp/userselect_groupaction_{group}.lock")
        for group in all_groups
    ]

    return locks


def apply_group_actions(
    model: UserSelectMarkupModel,
    user_acc: User,
    cur_user: User,
) -> None:
    if not model.actions:
        return
    if model.useActionQueues:
        apply_group_actions_queued(
            user_acc,
            cur_user,
            model.actions.addToGroups,
            model.actions.removeFromGroups,
            model.actions.changeGroup,
        )
    else:
        changed = apply_group_actions_locked(
            user_acc,
            cur_user,
            model.actions.addToGroups,
            model.actions.removeFromGroups,
            model.actions.changeGroup,
        )
        db.session.flush()

        if changed:
            sync_user_group_memberships_if_enabled(user_acc)


def apply_dist_actions(
    user_acc: User, model: UserSelectMarkupModel, actions: list[DistributeRightAction]
) -> list[str]:
    if model.useActionQueues:
        for dr in actions:
            register_dist_right_action(user_acc, dr)
        return []
    else:
        return apply_dist_right_actions(user_acc, actions)


@user_select_plugin.post("/apply")
def apply(
    username: str,
    task_id: str | None = None,
    par: GlobalParId | None = None,
    param: str | None = None,  # TODO: Use
) -> Response:
    model, cur_user, user_group, user_acc, doc = get_plugin_info(username, task_id, par)
    # No permissions to apply, simply return
    if not model.actions:
        return ok_response()

    log_user_select(
        f"[{cur_user.name}] apply on {user_acc.name} in {doc.path} (param = {param})"
    )

    locks = get_group_action_locks(model)
    for lock in locks:
        lock.acquire()

    try:
        apply_group_actions(model, user_acc, cur_user)

        right_dist_errors = apply_dist_actions(
            user_acc, model, model.actions.distributeRight
        )

        apply_field_actions(cur_user, user_acc, model.actions.setValue)

        session_verification_errors = apply_verify_session(
            "verify", user_acc, param, model.actions.verifyRemoteSessions
        )

        session_invalidation_errors = apply_verify_session(
            "invalidate", user_acc, param, model.actions.invalidateRemoteSessions
        )

        update_messages = apply_permission_actions(
            user_group,
            model.actions.addPermission,
            model.actions.removePermission,
            model.actions.confirmPermission,
            model.actions.changePermissionTime,
        )

        db.session.commit()
    finally:
        for lock in locks:
            lock.release()

    for msg in update_messages:
        log_right(msg)

    for error in right_dist_errors:
        log_warning(
            f"RIGHT_DIST: problem distributing rights for user {user_acc.email}: {error}"
        )

    for error in session_verification_errors:
        log_warning(
            f"SESSION_VERIFICATION: problem verifying session for user {user_acc.email}: {error}"
        )

    for error in session_invalidation_errors:
        log_warning(
            f"SESSION_VERIFICATION: problem invalidating session for user {user_acc.email}: {error}"
        )

    all_errors = (
        right_dist_errors + session_verification_errors + session_invalidation_errors
    )

    # Better throw an error here. This should prompt the user to at least try again
    # Unlike with undoing, it's better to get the user to reapply the rights or properly fix them
    # Moreover, this should encourage the user to report the problem with distribution ASAP
    if all_errors:
        raise RouteException("\n".join([f"* {error}" for error in all_errors]))

    return ok_response()


register_html_routes(
    user_select_plugin,
    class_schema(UserSelectHtmlModel, base_schema=DurationSchema),
    reqs_handler,
)
