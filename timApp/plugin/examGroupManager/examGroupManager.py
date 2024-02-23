import base64
import datetime
import json
import secrets
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Sequence

from flask import Response, request
from marshmallow import missing
from sqlalchemy import select, Row, delete

from timApp.auth.accesshelper import (
    get_doc_or_abort,
    verify_ownership,
    verify_admin,
    verify_logged_in,
)
from timApp.auth.logincodes.model import UserLoginCode
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.editing.globalparid import GlobalParId
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewRoute
from timApp.folder.folder import Folder
from timApp.item.deleting import soft_delete_document
from timApp.plugin.plugin import Plugin
from timApp.tim_app import app
from timApp.timdb.sqa import db, run_sql
from timApp.user.groups import (
    verify_groupadmin,
    do_create_group,
    verify_group_edit_access,
    verify_group_access,
)
from timApp.user.user import User, UserInfo, owner_access_set, manage_access_set
from timApp.user.usergroup import UserGroup, get_groups_by_ids
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.flask.requesthelper import (
    view_ctx_with_urlmacros,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import slugify
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
)
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
    event: bool = False
    timeslot: bool = False


@dataclass
class MemberViewOptionsMarkup:
    selectionControls: bool = True
    name: bool = True
    username: bool = False
    email: bool = False
    extraInfo: bool = True
    loginCode: bool = True


@dataclass
class ViewOptionsMarkup:
    groups: GroupViewOptionsMarkup = field(default_factory=GroupViewOptionsMarkup)
    members: MemberViewOptionsMarkup = field(default_factory=MemberViewOptionsMarkup)


@dataclass
class ExamGroupManagerMarkup(GenericMarkupModel):
    groupsPath: str | Missing = missing
    extraInfoTitle: str | Missing = missing
    showAllGroups: bool = False
    show: ViewOptionsMarkup = field(default_factory=ViewOptionsMarkup)
    groupNamePrefix: str | Missing = missing


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
    if not markup.groupsPath.startswith("groups"):
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
    return json_response(
        [
            {
                "id": g.id,
                "name": g.name,
                "readableName": ug_docs[admin_doc_id].title,
                "path": ug_docs[admin_doc_id].path,
                "isDirectOwner": curr_user in ug_docs[admin_doc_id].owners,
            }
            for g, admin_doc_id in groups_docs
        ]
    )


@exam_group_manager_plugin.post("/createGroup")
def create_group(
    name: str,
    group_folder_path: str,
    group_prefix: str = "",
) -> Response:
    group_name = f"{group_prefix}-{slugify(name)}"
    group_path = f"{group_folder_path}/{group_name}"
    ug, doc = do_create_group(group_path)
    doc.title = name
    db.session.commit()
    return json_response(
        ug.to_json()
        | {
            "readableName": doc.title,
            "isDirectOwner": True,
        }
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

    admin_docs = [g.admin_doc.docentries[0] for g in del_groups]

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


# FIXME: Review
@exam_group_manager_plugin.get("/members/<int:group_id>")
def get_members(group_id: int) -> Response:
    """
    :return: Group members of the specified group
    """

    check_usergroup_permissions(group_id)

    ug = UserGroup.get_by_id(group_id)
    if not ug:
        raise NotExist(f"Group with id {group_id} does not exist.")

    members: list[User] = ug.users
    data = []
    login_codes: Sequence[UserLoginCode] = (
        run_sql(
            select(UserLoginCode).filter(
                UserLoginCode.user_id.in_([u.id for u in members])
            )
        )
        .scalars()
        .all()
    )
    login_code_per_user = {lc.user_id: lc for lc in login_codes}
    for m in members:
        ulc: UserLoginCode = login_code_per_user.get(m.id, None)
        data.append(
            {
                "id": m.id,
                "name": m.name,
                "email": m.email,
                "real_name": m.real_name,
                # "extra_info": ulc.extra_info if ulc else None,
                "login_code": ulc.code if ulc else None,
            }
        )

    return json_response(status_code=200, jsondata=data)


# FIXME: Review
@exam_group_manager_plugin.post("/members/from_groups")
def get_members_from_groups() -> Response:
    """
    :return: Group members of all listed groups
    """

    r: dict = request.get_json()
    group_ids: list[int] = [*r.get("ids")]  # type: ignore

    ugds: list[UserGroupDoc] = list(
        run_sql(select(UserGroupDoc).where(UserGroupDoc.group_id.in_(group_ids)))
        .scalars()
        .all()
    )
    ug_docs = list(get_doc_or_abort(u.doc_id) for u in ugds)
    accessible_ugdocs: list[DocInfo] = list()

    for ugd in ug_docs:
        accessible_ugdocs.append(ugd) if (
            verify_ownership(ugd) or verify_admin() or verify_groupadmin()
        ) else None

    if not accessible_ugdocs:
        return json_response(
            status_code=403,
            jsondata={"result": {"ok": False, "error": f"Insufficient permissions."}},
        )

    ugd_ids: list[int] = list(ugd.id for ugd in accessible_ugdocs)
    accessible_group_ids: list[int] = list(
        g.group_id for g in ugds if g.doc_id in ugd_ids
    )

    ugs: list[UserGroup] = list(
        run_sql(select(UserGroup).where(UserGroup.id.in_(accessible_group_ids)))
        .scalars()
        .all()
    )

    members: list[User] = [u for ug in ugs for u in ug.users]
    # for ug in ugs:
    #     for u in ug.users:
    #         members.extend(u)

    data = [
        {
            "id": m.id,
            "name": m.name,
            "email": m.email,
            "real_name": m.real_name,
        }
        for m in members
    ]
    return json_response(status_code=200, jsondata=data)


# FIXME: Review
def check_usergroup_permissions(
    group_id: int, user: User | None = None, action: str | None = None
) -> None | Response:
    from timApp.user.groups import verify_groupadmin

    if not user:
        from timApp.auth.sessioninfo import get_current_user_object

        user = get_current_user_object()
    current_user = user
    ugd: UserGroupDoc = run_sql(select(UserGroupDoc).filter_by(group_id=group_id).limit(1)).scalars().first()  # type: ignore
    ug_doc = get_doc_or_abort(ugd.doc_id)

    if not verify_groupadmin(user=current_user, action=action) or not verify_ownership(
        b=ug_doc
    ):
        return json_response(
            status_code=403,
            jsondata={"result": {"ok": False, "error": "Insufficient permissions."}},
        )
    return None


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

    members: list[User] = list(source_group.users)
    added_memberships = []
    for u in members:
        u.add_to_group(target_group, cur_user)
        added_memberships.append(u.name)
    db.session.commit()
    return ok_response()


# FIXME: Review
@exam_group_manager_plugin.post("/addMember/<int:group_id>")
def create_users(
    group_id: int,
    given_name: str,
    surname: str,
    extra_info: str,
) -> Response:
    user = do_create_users(group_id, given_name, surname, extra_info)

    # TODO: Add extra info to the user group

    return json_response(user)


def do_create_users(group_id: int, given_name: str, surname: str, extra_info: str = ""):
    current_user = get_current_user_object()
    group: UserGroup = UserGroup.get_by_id(group_id)
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
    db.session.commit()
    return user


# FIXME: Review
@exam_group_manager_plugin.post("/importUsers/<int:group_id>")
def import_users_to_group(group_id: int) -> Response:
    group = UserGroup.get_by_id(group_id)
    if not group:
        raise NotExist(f"Group with ID {group_id} does not exist.")

    check_usergroup_permissions(group_id)
    text: str = request.get_json().get("text")
    # TODO sanitize json
    udata = text.splitlines()
    users: list[User] = []
    ugs: list[tuple[UserGroup, str]] = []
    for data in udata:
        einfo, lname, fname = data.split(" ")

        # dummy_uname: str = str(
        #     base64.urlsafe_b64encode(
        #         f"{einfo}{lname + fname}{time.time_ns()}".encode()
        #     ),
        #     encoding="utf-8",
        # )
        # dummy_pass: str = str(
        #     base64.urlsafe_b64encode(
        #         f"{einfo}{fname + lname}{time.time_ns()}".encode()
        #     ),
        #     encoding="utf-8",
        # )
        #
        # dummy_email: str = str(
        #     base64.urlsafe_b64encode(f"{einfo}{lname}{time.time_ns()}".encode()),
        #     encoding="utf-8",
        # )
        # dummy_email = f"{dummy_email}@example.com"
        #
        # ui: UserInfo = UserInfo(
        #     username=dummy_uname,
        #     given_name=fname,
        #     last_name=lname,
        #     full_name=f"{lname} {fname}",
        #     email=dummy_email,
        #     password=dummy_pass,
        # )
        #
        # user, ug = User.create_with_group(ui)
        # ugs.append((ug, einfo))

        user = do_create_users(group_id, lname, fname, einfo)
        users.append(user)

    # TODO find a way to get rid of db commits that are currently needed
    #      so we can get references to eg. new usergroup ids
    # db.session.flush()

    # For some reason Mypy gets confused here, so we will just ignore it for now
    data = list()  # type: ignore
    # from timApp.auth.sessioninfo import get_current_user_object
    # current_user = get_current_user_object()
    for user in users:
        # user.add_to_group(group, current_user)
        # ug = user.get_personal_group()
        data.append(  # type: ignore
            {
                "id": user.get_personal_group().id,
                "name": user.name,
                "email": user.email,
                "real_name": user.real_name,
            }
        )

    # db.session.commit()
    json_data = json.dumps(data)

    return json_response(
        status_code=200,
        jsondata=json_data,
    )


# FIXME: Review
@exam_group_manager_plugin.post("/generateCodes")
def generate_codes_for_members() -> Response:
    """
    Updates UserLoginCode properties.
    Note: this function will always overwrite previous values
    :return: Response
    """
    r: dict = request.get_json()
    members: list = [*r.get("members")]  # type: ignore

    # TODO time should be in server time (UTC+0), currently local time is used
    act_start = datetime.fromisoformat(str(r.get("activation_start")))
    act_end = datetime.fromisoformat(str(r.get("activation_end")))
    # act_status: int = int(r.get("activation_status"))

    for m in members:
        user_id: int = int(m.get("id"))
        extra_info = str(m.get("extra_info"))

        # TODO: Deactivate previous codes
        u = User.get_by_id(user_id)
        UserLoginCode.generate_new(
            user=u,
            active_from=act_start,
            active_to=act_end,
            name=extra_info,
        )

    db.session.commit()
    return ok_response()


def reqs_handle() -> PluginReqs:
    return PluginReqs(
        js=["timExamGroupManager"],
        multihtml=True,
    )


register_html_routes(
    exam_group_manager_plugin, class_schema(ExamGroupManagerHtmlModel), reqs_handle
)
