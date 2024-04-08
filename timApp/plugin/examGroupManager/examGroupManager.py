import json
import secrets
from collections import defaultdict
from dataclasses import dataclass, field, fields
from datetime import datetime, timedelta
from typing import Sequence, Iterable, Any, Callable

from flask import Response, request, render_template
from flask_babel import gettext
from marshmallow import missing, ValidationError
from marshmallow.fields import Field
from numpy import sign
from sqlalchemy import select, Row, delete, update, func

from timApp.answer.answer import Answer
from timApp.answer.answer_models import UserAnswer
from timApp.auth.accesshelper import (
    get_doc_or_abort,
    verify_ownership,
    verify_logged_in,
    has_teacher_access,
    AccessDenied,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.logincodes.model import UserLoginCode
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.editing.globalparid import GlobalParId
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewRoute
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.deleting import soft_delete_document
from timApp.plugin.plugin import Plugin
from timApp.tim_app import app
from timApp.timdb.sqa import db, run_sql
from timApp.user.groups import (
    do_create_group,
    verify_group_access,
)
from timApp.user.user import User, UserInfo, manage_access_set
from timApp.user.usergroup import UserGroup, get_groups_by_ids
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.user.usergroupmember import UserGroupMember, membership_current
from timApp.user.userutils import grant_access, expire_access
from timApp.util.flask.requesthelper import (
    view_ctx_with_urlmacros,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import (
    json_response,
    ok_response,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_info
from timApp.util.utils import slugify, get_current_time
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema, field_for_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
)
from tim_common.timjsonencoder import TimJsonEncoder
from tim_common.utils import Missing, DurationSchema

exam_group_manager_plugin = TypedBlueprint(
    "exam_group_manager_plugin", __name__, url_prefix="/examGroupManager"
)


@dataclass
class GroupViewOptionsMarkup:
    selectionControls: bool = True
    name: bool = True
    document: bool = False
    fullDocPath: bool = False
    memberCount: bool = True
    exam: bool = True
    timeslot: bool = False


@dataclass
class MemberViewOptionsMarkup:
    selectionControls: bool = True
    name: bool = True
    username: bool = False
    email: bool = False
    extraInfo: bool = True
    loginCode: bool = True
    extraTime: bool = True


@dataclass
class ViewOptionsMarkup:
    groups: GroupViewOptionsMarkup = field(default_factory=GroupViewOptionsMarkup)
    members: MemberViewOptionsMarkup = field(default_factory=MemberViewOptionsMarkup)


@dataclass
class Exam:
    docId: int
    name: str
    url: str | None = None


@dataclass
class ExamGroupManagerMarkup(GenericMarkupModel):
    groupsPath: str | Missing = missing
    extraInfoTitle: str | Missing = missing
    showAllGroups: bool = False
    show: ViewOptionsMarkup = field(default_factory=ViewOptionsMarkup)
    groupNamePrefix: str | Missing = missing
    exams: list[Exam] = field(default_factory=list)
    practiceExam: Exam | Missing = missing


ExamGroupManagerMarkupSchema = class_schema(
    ExamGroupManagerMarkup, base_schema=DurationSchema
)


@dataclass
class ExamGroupManagerStateModel:
    pass


@dataclass
class ExamGroupManagerInputModel:
    pass


@dataclass
class ExamGroupManagerHtmlModel(
    GenericHtmlModel[
        ExamGroupManagerInputModel, ExamGroupManagerMarkup, ExamGroupManagerStateModel
    ]
):
    def get_component_html_name(self) -> str:
        return "tim-exam-group-manager"

    def get_static_html(self) -> str:
        return "<div>Exam Group Manager</div>"


def _get_plugin_markup(par: GlobalParId) -> tuple[ExamGroupManagerMarkup, UserContext]:
    verify_logged_in()
    doc = get_doc_or_abort(par.doc_id)
    verify_ownership(doc)

    user_ctx = UserContext.from_one_user(get_current_user_object())
    view_ctx = view_ctx_with_urlmacros(ViewRoute.Unknown)

    plugin, _ = Plugin.from_global_par(par, user_ctx, view_ctx)
    model: ExamGroupManagerMarkup = ExamGroupManagerMarkupSchema().load(plugin.values)

    return model, user_ctx


def _verify_exam_group_access(
    ug: UserGroup, user: User | None = None, require: bool = True
) -> bool:
    # TODO: check that the group is an exam group
    return verify_group_access(ug, manage_access_set, user, require=require)


@dataclass
class ExamGroupDataGlobal:
    examDocId: int | Missing = field(default=missing)
    examState: int = 0
    currentExamDoc: int | None = None
    accessAnswersTo: datetime | None = None

    def to_json(self) -> dict:
        res = {}
        for f in fields(self):
            v = getattr(self, f.name)
            if v is not missing:
                res[f.name] = v
        return res


exam_group_data_fields: dict[str, Field] = {
    f.name: field_for_schema(f.type) for f in fields(ExamGroupDataGlobal)
}
EXAM_GROUP_DATA_GLOBAL_FIELDS = {f.name for f in fields(ExamGroupDataGlobal)}


@dataclass
class ExamGroupDataUser:
    extraInfo: str | Missing = field(default=missing)
    extraTime: bool | Missing = field(default=missing)

    def to_json(self) -> dict:
        res = {}
        for f in fields(self):
            v = getattr(self, f.name)
            if v is not missing:
                res[f.name] = v
        return res


exam_group_data_user_fields: dict[str, Field] = {
    f.name: field_for_schema(f.type) for f in fields(ExamGroupDataUser)
}
EXAM_GROUP_DATA_USER_FIELDS = {f.name for f in fields(ExamGroupDataUser)}


def _get_latest_fields_any(task_fields: Iterable[str]) -> Sequence[Answer]:
    latest_answers_sub = (
        select(func.max(Answer.id))
        .select_from(Answer)
        .filter(Answer.task_id.in_(task_fields))
        .group_by(Answer.task_id)
    )

    latest_globals: Sequence[Answer] = (
        run_sql(select(Answer).filter(Answer.id.in_(latest_answers_sub)))
        .scalars()
        .all()
    )

    return latest_globals


def _get_latest_fields_usergroup(
    task_fields: Iterable[str],
    user: User | None = None,
    user_group: UserGroup | None = None,
) -> dict[int, list[Answer]]:
    if not user and not user_group:
        raise ValueError("Either u or ug must be provided")
    if user:
        ug_users_sub: Any = [user.id]
    elif user_group:
        ug_users_sub = user_group.memberships.filter(membership_current).with_entities(
            UserGroupMember.user_id
        )

    latest_answers_sub = (
        select(
            UserAnswer.user_id.label("user_id"),
            func.max(Answer.id).label("answer_id"),
        )
        .select_from(Answer)
        .join(UserAnswer, UserAnswer.answer_id == Answer.id)
        .filter(Answer.task_id.in_(task_fields) & UserAnswer.user_id.in_(ug_users_sub))
        .group_by(UserAnswer.user_id, Answer.task_id)
        .subquery()
    )

    latest_users_stmt = (
        select(latest_answers_sub.c.user_id, Answer)
        .select_from(Answer)
        .join(latest_answers_sub, Answer.id == latest_answers_sub.c.answer_id)
    )

    result: dict[int, list[Answer]] = defaultdict(list)

    for uid, a in run_sql(latest_users_stmt).all():
        result[uid].append(a)

    return dict(result)


def _update_exam_group_data_global(ug: UserGroup, data: ExamGroupDataGlobal) -> None:
    doc = ug.admin_doc
    assert doc is not None

    glo_fields_to_update = {
        f for f in EXAM_GROUP_DATA_GLOBAL_FIELDS if getattr(data, f) is not missing
    }

    globals_dict: dict[str, Answer] = {
        a.task_id: a
        for a in _get_latest_fields_any(
            [f"{doc.id}.GLO_{f}" for f in glo_fields_to_update]
        )
    }

    for f in fields(data):
        if f.name not in glo_fields_to_update:
            continue
        new_json = json.dumps(
            {"c": json.dumps(getattr(data, f.name), cls=TimJsonEncoder)}
        )
        task_id = f"{doc.id}.GLO_{f.name}"
        ans = globals_dict.get(task_id, None)
        if not ans:
            ans = Answer(
                task_id=task_id,
                valid=True,
            )
            db.session.add(ans)
        ans.content = new_json


def _get_exam_group_data_global(ug: UserGroup) -> ExamGroupDataGlobal:
    doc = ug.admin_doc
    assert doc is not None

    result = ExamGroupDataGlobal()
    for a in _get_latest_fields_any(
        [f"{doc.id}.GLO_{f}" for f in EXAM_GROUP_DATA_GLOBAL_FIELDS]
    ):
        field_name = a.task_id.split(".")[-1][4:]
        data = json.loads(json.loads(a.content).get("c"))
        try:
            data_val = exam_group_data_fields[field_name].deserialize(data)
        except ValidationError:
            continue

        setattr(result, field_name, data_val)

    return result


def _get_exam_group_data_user(ug: UserGroup) -> dict[int, ExamGroupDataUser]:
    doc = ug.admin_doc
    assert doc is not None

    result: dict[int, ExamGroupDataUser] = {}
    for uid, answers in _get_latest_fields_usergroup(
        [f"{doc.id}.{f}" for f in EXAM_GROUP_DATA_USER_FIELDS], user_group=ug
    ).items():
        user_data = ExamGroupDataUser()
        for a in answers:
            field_name = a.task_id.split(".")[-1]
            data = json.loads(json.loads(a.content).get("c"))
            try:
                data_val = exam_group_data_user_fields[field_name].deserialize(data)
            except ValidationError:
                continue
            setattr(user_data, field_name, data_val)
        result[uid] = user_data

    return result


def _update_exam_group_data_user(
    u: User, ug: UserGroup, data: ExamGroupDataUser
) -> None:
    doc = ug.admin_doc
    assert doc is not None

    user_fields_to_update = {
        f for f in EXAM_GROUP_DATA_USER_FIELDS if getattr(data, f) is not missing
    }

    user_fields: list[Answer] = _get_latest_fields_usergroup(
        [f"{doc.id}.{f}" for f in user_fields_to_update], user=u
    ).get(u.id, [])

    answer_by_field = {a.task_id.split(".")[-1]: a for a in user_fields}

    for f in fields(data):
        if f.name not in user_fields_to_update:
            continue
        new_json = json.dumps(
            {"c": json.dumps(getattr(data, f.name), cls=TimJsonEncoder)}
        )
        task_id = f"{doc.id}.{f.name}"
        ans = answer_by_field.get(f.name, None)
        if not ans:
            ans = Answer(
                task_id=task_id,
                valid=True,
            )
            db.session.add(ans)
        ans.content = new_json
        ans.users_all = [u]


@exam_group_manager_plugin.get("/groups")
def get_groups(
    doc_id: int,
    par_id: str,
) -> Response:
    """
    Fetch exam groups that can be managed by the current user in the current plugin.

    :param doc_id: Document ID where the management plugin is located.
    :param par_id: Paragraph ID where the management plugin is located.
    :param show_all_groups: Whether to show all groups or only the ones the user has direct ownership of.
    :return: Groups that can be managed by the current user in the current manager.
    """
    markup, user_ctx = _get_plugin_markup(GlobalParId(doc_id, par_id))
    user = user_ctx.user
    curr_user: UserGroup = user.get_personal_group()

    if markup.groupsPath is missing:
        raise RouteException("groupsPath is missing from the plugin markup")

    fpath = markup.groupsPath
    if not fpath or isinstance(fpath, Missing) or not fpath.startswith("groups"):
        fpath = f"groups/{markup.groupsPath}"
    folder = Folder.find_by_path(fpath)
    if not folder:
        raise NotExist(f"Group folder {fpath} does not exist.")

    ug_docs: dict[int, DocInfo] = {d.id: d for d in folder.get_all_documents()}
    groups_docs: Sequence[Row[tuple[UserGroup, int]]] = (
        run_sql(
            select(UserGroup, UserGroupDoc.doc_id)
            .join(UserGroupDoc)
            .filter(UserGroupDoc.doc_id.in_([doc.id for doc in ug_docs.values()]))
        )
        .unique()
        .all()
    )
    group_member_counts: Sequence[Row[tuple[int, int]]] = run_sql(
        select(UserGroupMember.usergroup_id, func.count(UserGroupMember.user_id))
        .filter(
            UserGroupMember.usergroup_id.in_([g.id for g, _ in groups_docs])
            & membership_current
        )
        .group_by(UserGroupMember.usergroup_id)
    ).all()
    group_member_counts_dict = {gid: count for gid, count in group_member_counts}
    return json_response(
        [
            {
                "id": g.id,
                "name": g.name,
                "readableName": ug_docs[admin_doc_id].title,
                "path": ug_docs[admin_doc_id].path,
                "isDirectOwner": curr_user in ug_docs[admin_doc_id].owners,
                "memberCount": group_member_counts_dict.get(g.id, 0),
                **_get_exam_group_data_global(g).to_json(),
            }
            for g, admin_doc_id in groups_docs
        ]
    )


@exam_group_manager_plugin.post("/createGroup")
def create_group(
    name: str,
    group_folder_path: str,
    group_prefix: str = "",
    exam_doc_id: int | None = None,
) -> Response:
    group_name = (
        f"{slugify(group_prefix)}-{slugify(name)}-{slugify(secrets.token_urlsafe(8))}"
    )
    group_path = f"{group_folder_path}/{group_name}"
    ug, doc = do_create_group(group_path)
    doc.title = name

    extra_data = {}
    if exam_doc_id:
        extra_data = {"examDocId": exam_doc_id}
        _update_exam_group_data_global(ug, ExamGroupDataGlobal(examDocId=exam_doc_id))

    db.session.commit()
    return json_response(
        ug.to_json()
        | {
            "readableName": doc.title,
            "isDirectOwner": True,
        }
        | extra_data
    )


@exam_group_manager_plugin.post("/deleteGroup")
def delete_exam_group(group_id: int) -> Response:
    """Route for deleting an exam group.
    Permanently deletes a UserGroup from the database.
    When calling this function, user should be notified that the operation cannot be undone.

    The group documents will remain in the TIM 'trash' folder, but without database entries they
    should not be able to reference any data.

    :param group_id: ID of the group that should be deleted
    """
    do_delete_exam_groups([group_id])
    db.session.commit()
    return ok_response()


def do_delete_exam_groups(group_ids: list[int]) -> None:
    del_groups = get_groups_by_ids(group_ids)

    for g in del_groups:
        _verify_exam_group_access(g)

    admin_docs = [g.admin_doc.docentries[0] for g in del_groups if g.admin_doc]

    run_sql(
        delete(UserGroupDoc)
        .where(UserGroupDoc.group_id.in_(group_ids))
        .execution_options(synchronize_session="auto")
    )

    run_sql(
        delete(UserGroupMember)
        .where(UserGroupMember.usergroup_id.in_(group_ids))
        .execution_options(synchronize_session="auto")
    )

    run_sql(
        delete(UserGroup)
        .where(UserGroup.id.in_(group_ids))
        .execution_options(synchronize_session="auto")
    )

    # Flush to trigger any database errors
    db.session.flush()

    for doc in admin_docs:
        soft_delete_document(doc)


def _get_current_login_codes(ug: UserGroup) -> Sequence[UserLoginCode]:
    return (
        run_sql(
            select(UserLoginCode).filter(
                UserLoginCode.user_id.in_([u.id for u in ug.users])
                & (UserLoginCode.name == ug.name)
                & (UserLoginCode.valid.is_(True))
            )
        )
        .scalars()
        .all()
    )


def _get_exam_group_members_json(ug: UserGroup) -> list[dict]:
    data = []
    login_codes = _get_current_login_codes(ug)
    login_code_per_user = {lc.user_id: lc for lc in login_codes}
    exam_group_fields_per_user = _get_exam_group_data_user(ug)

    for m in ug.users:
        ulc = login_code_per_user.get(m.id, None)
        exam_fields = exam_group_fields_per_user.get(m.id, None)
        exam_fields_json = exam_fields.to_json() if exam_fields else {}
        data.append(
            {
                "id": m.id,
                "name": m.name,
                "email": m.email,
                "real_name": m.real_name,
                "login_code": ulc.code if ulc else None,
                **exam_fields_json,
            }
        )

    return data


@exam_group_manager_plugin.get("/members/<int:group_id>")
def get_members(group_id: int) -> Response:
    """
    :return: Group members of the specified group
    """
    ug = UserGroup.get_by_id(group_id)
    if not ug:
        raise NotExist(f"Group with id {group_id} does not exist.")

    _verify_exam_group_access(ug)

    data = _get_exam_group_members_json(ug)

    return json_response(status_code=200, jsondata=data)


@exam_group_manager_plugin.post("/copyMembers")
def copy_members(from_id: int, to_id: int) -> Response:
    """
    Copies group memberships from one UserGroup to another.

    Note: this function is intended to be used in conjunction with the group-management component,
    see `timApp/static/scripts/tim/ui/group-management.component.ts`. We should probably limit
    the database queries to only the base64-encoded names, since the group-management
    component is currently configured to produce such names by default.
    :param from_id: source UserGroup ID
    :param to_id: target UserGroup ID
    :return: Response with added member names, or error message
    """
    cur_user = get_current_user_object()
    if from_id == to_id:
        raise RouteException("Cannot copy members from a group to itself.")

    source_group = UserGroup.get_by_id(from_id)
    if not source_group:
        raise NotExist(f"Group with ID {from_id} does not exist.")

    target_group = UserGroup.get_by_id(to_id)
    if not target_group:
        raise NotExist(f"Group with ID {to_id} does not exist.")

    _verify_exam_group_access(source_group)
    _verify_exam_group_access(target_group)

    # TODO: Copy extra infos

    members: list[User] = list(source_group.users)
    added_memberships = []
    for u in members:
        u.add_to_group(target_group, cur_user)
        added_memberships.append(u.name)
    db.session.commit()
    return ok_response()


@exam_group_manager_plugin.post("/addMember/<int:group_id>")
def create_users(
    group_id: int,
    given_name: str,
    surname: str,
    extra_info: str,
) -> Response:
    user, extra_data = do_create_users(group_id, given_name, surname, extra_info)
    return json_response(user.to_json() | extra_data.to_json())


def do_create_users(
    group_id: int, given_name: str, surname: str, extra_info: str = ""
) -> tuple[User, ExamGroupDataUser]:
    current_user = get_current_user_object()
    group = UserGroup.get_by_id(group_id)
    if not group:
        raise NotExist(f"Group with ID {group_id} does not exist.")
    if not given_name.strip():
        raise RouteException("Name is required.")
    if not surname.strip():
        raise RouteException("Surname is required.")
    # Check permission for the group
    # Only users with manage access to the group can create and add new members
    _verify_exam_group_access(group, current_user)
    no_reply_email_name, no_reply_email_domain = app.config["NOREPLY_EMAIL"].split(
        "@", 1
    )
    while True:
        username = (
            f"{slugify(given_name)}.{slugify(surname)}_{secrets.token_urlsafe(16)}"
        )
        email = f"{no_reply_email_name}+{username}@{no_reply_email_domain}"
        if not User.get_by_name(username):
            break
    ui: UserInfo = UserInfo(
        username=username,
        given_name=given_name,
        last_name=surname,
        full_name=f"{surname} {given_name}",
        email=email,
    )
    user, _ = User.create_with_group(ui)
    user.add_to_group(group, current_user)
    # Flush to make user be member of the group
    db.session.flush()

    extra_data = ExamGroupDataUser(extraInfo=extra_info)
    _update_exam_group_data_user(user, group, extra_data)

    db.session.commit()
    return user, extra_data


@exam_group_manager_plugin.post("/importUsers/<int:group_id>")
def import_users_to_group(group_id: int, text: str) -> Response:
    group = UserGroup.get_by_id(group_id)
    if not group:
        raise NotExist(f"Group with ID {group_id} does not exist.")

    _verify_exam_group_access(group)

    udata = text.splitlines()
    users: list[User] = []
    ugs: list[tuple[UserGroup, str]] = []
    for data in udata:
        data = data.strip()
        if not data:
            continue
        parts = data.split(" ", 2)
        if len(parts) < 3:
            raise RouteException(
                gettext(
                    "Invalid student row: %(data)s. Expected format: <grade> <surname> <given_names>",
                    data=data,
                )
            )
        einfo, lname, fname = parts

        user, _ = do_create_users(group_id, lname, fname, einfo)
        users.append(user)

    return ok_response()


@exam_group_manager_plugin.post("/generateCodes")
def generate_codes_for_members(group_id: int) -> Response:
    """
    Updates UserLoginCode properties.
    Note: this function will always overwrite previous values
    :return: Response
    """
    ug = UserGroup.get_by_id(group_id)
    if not ug:
        raise NotExist(f"Group with ID {group_id} does not exist.")

    _verify_exam_group_access(ug)

    ug_members = list(ug.users)

    # Disable previous codes of the members
    # TODO: Instead provide option on whether to disable previous codes
    run_sql(
        update(UserLoginCode)
        .where(UserLoginCode.user_id.in_([u.id for u in ug_members]))
        .values(active_to=datetime.now(), valid=False)
    )
    db.session.flush()

    for m in ug_members:
        UserLoginCode.generate_new(
            user=m,
            active_from=None,
            active_to=datetime.now(),
            name=ug.name,
        )

    db.session.commit()
    return ok_response()


@exam_group_manager_plugin.get("/printCodes/<int:group_id>")
def print_login_codes(
    group_id: int,
    doc_id: int,
    par_id: str,
    practice: bool = False,
) -> str:
    ug = UserGroup.get_by_id(group_id)
    if not ug:
        raise NotExist(f"Group with id {group_id} does not exist.")

    _verify_exam_group_access(ug)
    extra_data = _get_exam_group_data_global(ug)
    users = _get_exam_group_members_json(ug)

    plugin, _ = _get_plugin_markup(GlobalParId(doc_id, par_id))
    if practice and not plugin.practiceExam:
        raise NotExist(gettext("Practice exam not set in the plugin markup."))

    exams_by_doc_id = {e.docId: e for e in plugin.exams}

    exam = (
        exams_by_doc_id.get(extra_data.examDocId, None)
        if not practice and not isinstance(extra_data.examDocId, Missing)
        else plugin.practiceExam
    )
    exam_url: str | None = None
    exam_title: str | None = None
    if exam and not isinstance(exam, Missing):
        exam_url = exam.url
        exam_title = exam.name

    if not exam_url or not exam_title:
        assert not isinstance(extra_data.examDocId, Missing)
        doc = get_doc_or_abort(extra_data.examDocId)
        if not exam_url:
            exam_url = doc.get_url_for_view("view")
        if not exam_title:
            exam_title = doc.title

    admin_block: Block | None = ug.admin_doc
    assert admin_block is not None
    admin_doc: DocEntry = admin_block.docentries[0]
    group_name: str = admin_doc.title

    return render_template(
        "examgroupmanager/print_codes.jinja2",
        users=users,
        exam_url=exam_url,
        group_name=group_name,
        exam_title=exam_title,
    )


LOGIN_CODE_ACTIVE_DURATION = timedelta(days=30)


def _enable_login_codes(
    ug: UserGroup, extra: ExamGroupDataGlobal, log: bool = True
) -> None:
    login_codes = list(_get_current_login_codes(ug))
    if not login_codes:
        raise RouteException(
            gettext(
                "No login codes found for the group. Generate login codes before starting the exam."
            )
        )

    now = get_current_time()
    for lc in login_codes:
        lc.active_from = now
        lc.active_to = now + LOGIN_CODE_ACTIVE_DURATION

    if log:
        u = get_current_user_object()
        doc = _get_current_exam_doc(extra)
        log_info(
            f"ExamGroupManage: {u.name} enabled login codes for group {ug.name} (exam doc: {doc.path})"
        )


def _disable_login_codes(
    ug: UserGroup, extra: ExamGroupDataGlobal, log: bool = True
) -> None:
    login_codes = list(_get_current_login_codes(ug))
    if not login_codes:
        return

    now = get_current_time()
    for lc in login_codes:
        lc.active_from = None
        lc.active_to = now

    if log:
        u = get_current_user_object()
        doc = _get_current_exam_doc(extra)
        log_info(
            f"ExamGroupManage: {u.name} disabled login codes for group {ug.name} (exam doc: {doc.path})"
        )


def _get_current_exam_doc(extra: ExamGroupDataGlobal) -> DocInfo:
    doc_id = extra.currentExamDoc
    if not doc_id:
        raise RouteException(
            gettext(
                "The group is currently not in an exam. Please set the exam document first."
            )
        )
    doc = get_doc_or_abort(doc_id)

    # Require just teacher access, since teachers should not have high rights to the exam document
    # FIXME: Additional flags for exam management, since this is lower than the manage right
    if not has_teacher_access(doc):
        raise AccessDenied(gettext("You do not have access to the exam document."))

    return doc


def _begin_exam(ug: UserGroup, extra: ExamGroupDataGlobal) -> None:
    doc = _get_current_exam_doc(extra)
    # TODO: Maybe set duration?
    for u in ug.users:  # type: User
        grant_access(u.get_personal_group(), doc, AccessType.view)

    cur_u = get_current_user_object()
    log_info(
        f"ExamGroupManage: {cur_u.name} began exam for group {ug.name} (exam doc: {doc.path})"
    )


def _interrupt_exam(ug: UserGroup, extra: ExamGroupDataGlobal) -> None:
    doc = _get_current_exam_doc(extra)
    for u in ug.users:  # type: User
        expire_access(u.get_personal_group(), doc, AccessType.view)

    cur_u = get_current_user_object()
    log_info(
        f"ExamGroupManage: {cur_u.name} interrupted exam for group {ug.name} (exam doc: {doc.path})"
    )


def _end_exam_main_group(ug: UserGroup, extra: ExamGroupDataGlobal) -> None:
    doc = _get_current_exam_doc(extra)
    extra_data_by_uid = _get_exam_group_data_user(ug)

    for u in ug.users:  # type: User
        extra_data = extra_data_by_uid[u.id]

        if not extra_data.extraTime:
            expire_access(u.get_personal_group(), doc, AccessType.view)

    cur_u = get_current_user_object()
    log_info(
        f"ExamGroupManage: {cur_u.name} ended exam for main students in group {ug.name} (exam doc: {doc.path})"
    )


def _resume_exam_main_group(ug: UserGroup, extra: ExamGroupDataGlobal) -> None:
    doc = _get_current_exam_doc(extra)

    extra_data_by_uid = _get_exam_group_data_user(ug)

    for u in ug.users:  # type: User
        extra_data = extra_data_by_uid[u.id]

        if not extra_data.extraTime:
            grant_access(u.get_personal_group(), doc, AccessType.view)

    cur_u = get_current_user_object()
    log_info(
        f"ExamGroupManage: {cur_u.name} resumed exam for main students in group {ug.name} (exam doc: {doc.path})"
    )


def _end_exam_extra_time(ug: UserGroup, extra: ExamGroupDataGlobal) -> None:
    doc = _get_current_exam_doc(extra)

    extra_data_by_uid = _get_exam_group_data_user(ug)

    for u in ug.users:  # type: User
        extra_data = extra_data_by_uid[u.id]

        if extra_data.extraTime:
            expire_access(u.get_personal_group(), doc, AccessType.view)

    cur_u = get_current_user_object()
    log_info(
        f"ExamGroupManage: {cur_u.name} ended exam for students with extra time in group {ug.name} (exam doc: {doc.path})"
    )


def _resume_exam_extra_time(ug: UserGroup, extra: ExamGroupDataGlobal) -> None:
    doc = _get_current_exam_doc(extra)

    extra_data_by_uid = _get_exam_group_data_user(ug)

    for u in ug.users:  # type: User
        extra_data = extra_data_by_uid[u.id]

        if extra_data.extraTime:
            grant_access(u.get_personal_group(), doc, AccessType.view)

    cur_u = get_current_user_object()
    log_info(
        f"ExamGroupManage: {cur_u.name} resumed exam for students with extra time in group {ug.name} (exam doc: {doc.path})"
    )


def _do_nothing(*_: Any) -> None:
    pass


_state_apply_functions: list[Callable[[UserGroup, ExamGroupDataGlobal], None]] = [
    _enable_login_codes,  # 0 -> 1, activate login codes
    _do_nothing,  # 1 -> 2, students log in
    _do_nothing,  # 2 -> 3, check that students log in
    _begin_exam,  # 3 -> 4, begin exam
    _end_exam_main_group,  # 4 -> 5, end exam for main students
    _end_exam_extra_time,  # 5 -> 6, end exam for students with extra time
]

_state_revert_functions: list[Callable[[UserGroup, ExamGroupDataGlobal], None]] = [
    _do_nothing,  # 0 -> 0, needed so that the range() loop works
    _disable_login_codes,  # 1 -> 0, deactivate login codes
    _do_nothing,  # 2 -> 1, check
    _do_nothing,  # 3 -> 2, check
    _interrupt_exam,  # 4 -> 3, interrupt exam
    _resume_exam_main_group,  # 5 -> 4, resume exam for main students
    _resume_exam_extra_time,  # 6 -> 5, resume exam for students with extra time
]


def _set_exam_state_impl(
    ug: UserGroup, exam_group_data: ExamGroupDataGlobal, new_state: int
) -> None:
    new_state = max(0, min(len(_state_apply_functions), new_state))

    direction: int = sign(new_state - exam_group_data.examState)
    if direction == 0:
        return

    step_functions = (
        _state_apply_functions if direction > 0 else _state_revert_functions
    )

    for i in range(exam_group_data.examState, new_state, direction):
        step_functions[i](ug, exam_group_data)

    exam_group_data.examState = new_state
    _update_exam_group_data_global(ug, exam_group_data)


@exam_group_manager_plugin.post("/setExamState")
def set_exam_state(group_id: int, new_state: int) -> Response:
    ug = UserGroup.get_by_id(group_id)
    if not ug:
        raise NotExist(f"Group with ID {group_id} does not exist.")

    _verify_exam_group_access(ug)
    exam_group_data = _get_exam_group_data_global(ug)

    if exam_group_data.accessAnswersTo:
        now = get_current_time()
        if now < exam_group_data.accessAnswersTo:
            raise RouteException(
                gettext(
                    "Cannot change exam state while the answers are revealed. Disable answer reveal in section 4."
                )
            )

    _set_exam_state_impl(ug, exam_group_data, new_state)

    db.session.commit()
    return ok_response()


@exam_group_manager_plugin.post("/setExamDoc")
def set_exam_doc(group_id: int, exam_doc: int | None) -> Response:
    ug = UserGroup.get_by_id(group_id)
    if not ug:
        raise NotExist(f"Group with ID {group_id} does not exist.")

    _verify_exam_group_access(ug)
    exam_group_data = _get_exam_group_data_global(ug)

    if exam_doc is not None:
        doc = get_doc_or_abort(exam_doc)
        if not has_teacher_access(doc):
            raise AccessDenied(
                gettext(
                    "You do not have sufficient access to the exam document to set it."
                )
            )

        if (
            exam_group_data.currentExamDoc
            and exam_group_data.currentExamDoc != exam_doc
        ):
            _set_exam_state_impl(ug, exam_group_data, 0)

    exam_group_data.currentExamDoc = exam_doc
    _update_exam_group_data_global(ug, exam_group_data)

    db.session.commit()
    return json_response(exam_group_data.to_json())


@exam_group_manager_plugin.post("/updateMemberInfo")
def update_member_info(
    group_id: int,
    user_id: int,
    extra_time: bool | None = field(default=None, metadata={"data_key": "extraTime"}),
) -> Response:
    ug = UserGroup.get_by_id(group_id)
    if not ug:
        raise NotExist(f"Group with ID {group_id} does not exist.")

    _verify_exam_group_access(ug)

    user_group_data = _get_exam_group_data_user(ug)

    if user_id not in user_group_data:
        raise NotExist(f"User with ID {user_id} is not in the group")

    user_data = user_group_data[user_id]
    if extra_time is not None:
        user_data.extraTime = extra_time

    u = User.get_by_id(user_id)
    assert u is not None
    _update_exam_group_data_user(u, ug, user_data)

    db.session.commit()
    return json_response(user_data.to_json())


@exam_group_manager_plugin.post("/toggleAnswerReview")
def set_answer_review(group_id: int, state: bool) -> Response:
    ug = UserGroup.get_by_id(group_id)
    if not ug:
        raise NotExist(f"Group with ID {group_id} does not exist.")

    _verify_exam_group_access(ug)
    exam_group_data = _get_exam_group_data_global(ug)

    if exam_group_data.examState > 0:
        raise RouteException(
            gettext(
                "Cannot reveal answers while the exam is ongoing. Stop the exam and disable the login codes in the "
                "section 3."
            )
        )

    if isinstance(exam_group_data.examDocId, Missing):
        raise RouteException("No exam document set for the group.")

    doc = DocEntry.find_by_id(exam_group_data.examDocId)
    if not doc:
        raise NotExist(
            f"Exam document with ID {exam_group_data.examDocId} does not exist."
        )

    if state:
        now = get_current_time()
        active_to = now + timedelta(hours=1)
        _enable_login_codes(ug, exam_group_data, log=False)
        for u in ug.users:
            grant_access(
                u.get_personal_group(),
                doc,
                AccessType.view,
                accessible_from=now,
                accessible_to=active_to,
                restricted_mode=True,
            )
        exam_group_data.accessAnswersTo = active_to
    else:
        _disable_login_codes(ug, exam_group_data, log=False)
        for u in ug.users:
            expire_access(u.get_personal_group(), doc, AccessType.view)
        exam_group_data.accessAnswersTo = None

    _update_exam_group_data_global(ug, exam_group_data)

    u = get_current_user_object()
    log_info(
        f"ExamGroupManage: {u.name} {'enabled' if state else 'disabled'} answer review for group {ug.name} (exam doc: {doc.path})"
    )

    db.session.commit()
    return json_response({"accessAnswersTo": exam_group_data.accessAnswersTo})


def reqs_handle() -> PluginReqs:
    return PluginReqs(
        js=["timExamGroupManager"],
        multihtml=True,
    )


register_html_routes(
    exam_group_manager_plugin, class_schema(ExamGroupManagerHtmlModel), reqs_handle
)
