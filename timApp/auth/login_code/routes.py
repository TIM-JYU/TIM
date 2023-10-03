from flask import Response
from sqlalchemy import select

from timApp.auth.accesshelper import get_doc_or_abort, verify_ownership
from timApp.folder.folder import Folder
from timApp.timdb.sqa import run_sql
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.document.docsettings import DocSettings
from timApp.util.logger import log_info, log_debug

login_code = TypedBlueprint("login_code", __name__, url_prefix="/loginCode")


@login_code.get("/managers/<list[str]: groupnames>")
def get_groups_by_names(groupnames: list[str]) -> Response:
    """
    :return: UserGroup matching the provided group name
    """
    groups = UserGroup.get_groups_by_names(groupnames)
    group_doc_paths = [
        get_doc_or_abort(group.admin_doc.doc_id).path for group in groups
    ]
    return json_response(
        status_code=200,
        jsondata={
            [
                {"id": g.id, "name": g.name, "path": g_doc.path}
                for g, g_doc in (groups, group_doc_paths)
            ]
        },
    )
    # return json_response(status_code=400, jsondata={"error": "Could not find group: '" + groupname + "'"})


@login_code.get("/checkOwner/<int: doc_id>")
def check_ownership(doc_id: int) -> Response:
    from timApp.tim import get_current_user_object

    log_info(f"Checking document ownership with user: {get_current_user_object().name}")
    doc = get_doc_or_abort(doc_id=doc_id)
    log_info(f"  for document {{id: {doc.id}, title: {doc.title}, path: {doc.path}}}")
    has_ownership = verify_ownership(doc)
    return json_response({"has_ownership": has_ownership is not None})


@login_code.get("/checkRequest/<int: doc_id")
def debug_dialog(doc_id: int) -> Response:
    return json_response("Request success!")


@login_code.get("/groups")
def get_groups() -> Response:
    """
    Fetch all UserGroups from the folder specified with document setting `groupManagement: groupsPath: <str>`
    :return:
    """
    path = DocSettings.group_management_settings().get("groups_path")
    usergroup_ids = [g.id for g in Folder.get_all_documents(relative_paths=[path])]
    groups: list[UserGroup] = (
        run_sql(
            select(UserGroup).filter(
                UserGroupDoc.doc_id.in_(usergroup_ids) & UserGroup.id
                == UserGroupDoc.group_id
            )
        )
        .scalars()
        .all()
    )
    data = [{"id": g.id, "name": g.name} for g in groups]
    return json_response(data)
