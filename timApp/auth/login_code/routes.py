import base64
import json
import time
from base64 import b64decode, standard_b64decode
from flask import Response, request
from sqlalchemy import select

from timApp.auth.accesshelper import (
    get_doc_or_abort,
    verify_ownership,
    get_origin_from_request,
)
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup, get_groups_by_names
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.document.docsettings import DocSettings
from timApp.util.logger import log_info, log_debug, tim_logger

login_code = TypedBlueprint("login_code", __name__, url_prefix="/loginCode")


# TODO do we even need this route? we already check for document ownership.
@login_code.get("/managers/<int:doc_id>")
def get_managers(doc_id: int) -> Response:
    """
    :return: UserGroups that are listed as managers in the docsetting `groupManagement`
    """
    mgmnt_doc = get_doc_or_abort(doc_id).document
    groupnames: list[str] = mgmnt_doc.get_settings().group_management_settings()[
        "managers"
    ]

    groups = get_groups_by_names(groupnames)

    if not groups:
        return json_response(
            status_code=404, jsondata={"error": f"Could not find groups: {groupnames}"}
        )
    group_doc_paths = [
        # FIXME This is kind of convoluted, just to avoid Mypy errors
        get_doc_or_abort(group.admin_doc.id if group.admin_doc else -1).path
        for group in groups
    ]
    data = [
        {"id": g.id, "name": g.name, "path": path}
        for g, path in zip(groups, group_doc_paths)
    ]
    return json_response(status_code=200, jsondata=data)


@login_code.get("/groups/<int:doc_id>")
def get_groups(doc_id: int) -> Response:
    """
    Fetch all UserGroups from the folder specified with document setting `groupManagement: groupsPath: <str>`
    :return:
    """

    show_all_groups: str | None = request.args.get("showAllGroups")
    mgmnt_doc = get_doc_or_abort(doc_id).document
    # TODO currently only one group path should be taken into account
    path: list[str] = mgmnt_doc.get_settings().group_management_settings()["groupsPath"]
    fpath = None
    if not path[0].startswith("groups"):
        fpath = f"groups/{path[0]}"
    folder = Folder.find_by_path(fpath if fpath else path[0])
    if not folder:
        return json_response(
            status_code=404,
            jsondata={"error": f"Could not find any groups in path(s): {path}"},
        )

    ug_docs: list[DocInfo] = folder.get_all_documents()
    if not show_all_groups or show_all_groups == "false":
        from timApp.tim import get_current_user_object

        curr_user: UserGroup = get_current_user_object().get_personal_group()
        tmp = [ug for ug in ug_docs if curr_user in ug.owners]
        ug_docs = tmp

    # Mypy doesn't recognize the correct typing here :(
    groups = (
        run_sql(
            select(UserGroup).filter(
                UserGroupDoc.doc_id.in_([doc.id for doc in ug_docs])
                & (UserGroup.id == UserGroupDoc.group_id)
            )
        )
        .scalars()
        .all()
    )

    data = []
    for g in groups:
        group_name = decode_name(g.name)
        data.append(
            {
                "id": g.id,
                "name": group_name,
                "path": f"{folder.get_full_path()}/{group_name}",
            }
        )

    return json_response(status_code=200, jsondata=data)


@login_code.get("/members/<int:group_id>")
def get_members(group_id: int) -> Response:
    """
    :return: Group members of the specified group
    """
    ug: UserGroup | None = (
        run_sql(select(UserGroup).filter_by(id=group_id).limit(1)).scalars().first()
    )

    if not ug:
        return json_response(
            status_code=404, jsondata={"msg": f"Could not find group: {group_id}"}
        )
    members: list[User] = ug.users
    data = [
        {
            "id": m.id,
            "name": m.name,
            "email": m.email,
            "real_name": m.real_name,
            # "student_id": m.get_home_org_student_id(),
        }
        for m in members
    ]
    return json_response(status_code=200, jsondata=data)


@login_code.get("/checkOwner/<int:doc_id>")
def check_ownership(doc_id: int) -> Response:
    from timApp.tim import get_current_user_object

    log_info(f"Checking document ownership with user: {get_current_user_object().name}")
    doc = get_doc_or_abort(doc_id=doc_id)
    log_info(f"  for document {{id: {doc.id}, title: {doc.title}, path: {doc.path}}}")
    has_ownership = verify_ownership(doc)
    if has_ownership is not None:
        return json_response(
            status_code=200, jsondata={"has_ownership": has_ownership is not None}
        )
    else:
        return json_response(status_code=403, jsondata={})


@login_code.get("/checkRequest")
def debug_dialog() -> Response:
    log_info(f"Checking http request")
    return json_response(status_code=200, jsondata={"result": "Request success!"})


def decode_name(name: str) -> str:
    """
    Decode a group name encoded in base64 and return the plain name.
    Separates the originally given group name from the group document path and the timestamp.
    The encoded string has the following format (without the curly brackets):

        `{path/}{plain group name}_{timestamp}`, where:

            `path` is optional (but should default to 'groupsPath' if set in groupManagement document settings),

            `plain group name` is the name given to the group originally,

            `timestamp` is a server timestamp of the group's time of creation measured in milliseconds.

    The string is prefixed with the marker 'b64_' to signify that the group name is in encoded form.

    :param name: The encoded group name used internally by TIM
    :returns: Human-readable group name displayed to the user.
    """
    if name.startswith("b64_"):
        group_name = name.removeprefix("b64_")
        decoded = str(standard_b64decode(group_name.encode()), encoding="utf-8")
        return decoded.rsplit("/", maxsplit=1)[-1].rsplit("_", maxsplit=1)[0]
    return name


@login_code.post("/addMembers/<group_name>")
def create_users(group_name: str) -> Response:
    # TODO get user data from request
    # TODO create UserInfo object
    # TODO call user.create_with_group with UserInfo
    # TODO get internal group name from plain group name param
    # TODO add user to internal group
    # TODO commit to database
    from timApp.tim import get_current_user_object

    # req_json = request.get_json()
    # u = json.loads(req_json)
    u: dict = request.get_json()
    username: str = u.get("username")
    given_name: str = u.get("given_name")
    surname: str = u.get("surname")
    email: str = u.get("email")

    # Create dummy password (for now), we do not want teacher-managed users to have
    # their personal passwords as we are using temporary login codes.
    dummy_pass: str = str(
        base64.standard_b64encode(f"{email}{username}{time.time_ns()}".encode()),
        encoding="utf-8",
    )

    ui: UserInfo = UserInfo(
        username=username,
        given_name=given_name,
        last_name=surname,
        full_name=f"{surname} {given_name}",
        email=email,
        password=dummy_pass,
    )

    user, _ = User.create_with_group(ui)

    groups = (
        run_sql(
            select(UserGroup).where(
                (UserGroup.name == group_name) | UserGroup.name.like("b64_%")
            )
        )
        .scalars()
        .all()
    )
    group = None
    for g in groups:
        dec = decode_name(g.name)
        if g.name == group_name or dec == group_name:
            group = g
            break
    if not group:
        return json_response(
            status_code=404,
            jsondata={"result": {"error": f"No matching group: {group_name}."}},
        )

    user.add_to_group(group, get_current_user_object())
    db.session.commit()
    return json_response(
        status_code=200,
        jsondata={
            "id": user.id,
            "name": user.name,
            "email": user.email,
            "real_name": user.real_name,
        },
    )
