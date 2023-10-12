from flask import Response
from sqlalchemy import select

from timApp.auth.accesshelper import get_doc_or_abort, verify_ownership
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.timdb.sqa import run_sql
from timApp.user.usergroup import UserGroup, get_groups_by_names
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.document.docsettings import DocSettings
from timApp.util.logger import log_info, log_debug, tim_logger

login_code = TypedBlueprint("login_code", __name__, url_prefix="/loginCode")


@login_code.get("/managers/<int:doc_id>")
def get_managers(doc_id: int) -> Response:
    """
    :return: UserGroups that are listed as managers in the docsetting `groupManagement`
    """
    management_doc = get_doc_or_abort(doc_id).document
    groupnames: list[str] = management_doc.get_settings().group_management_settings()[
        "managers"
    ]
    # TODO should notify user if some managers (groups/users) do not exist

    groups = get_groups_by_names(groupnames)
    # TODO fetch usergroupdoc ids from db
    # groupdoc_ids: list[int] = run_sql(select(UserGroupDoc.doc_id).filter())
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


@login_code.get("/groups/<int:doc_id>")
def get_groups(doc_id: int) -> Response:
    """
    Fetch all UserGroups from the folder specified with document setting `groupManagement: groupsPath: <str>`
    :return:
    """
    management_doc = get_doc_or_abort(doc_id).document
    # TODO currently only one group path should be taken into account
    path: list[str] = management_doc.get_settings().group_management_settings()[
        "groupsPath"
    ]
    fpath = None
    if not path[0].startswith("groups"):
        fpath = f"groups/{path[0]}"
    folder = Folder.find_by_path(fpath if fpath else path[0])
    # TODO should we just compare group doc names to UserGroup names,
    #      since UserGroup names are unique? Current implementation is
    #      due to the group id being the primary key.
    ug_docs = [doc for doc in folder.get_all_documents()]

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

    data = [
        {"id": g.id, "name": g.name, "path": f"{folder.path}/{g.name}"} for g in groups
    ]
    return json_response(status_code=200, jsondata=data)


@login_code.get("/checkOwner/<doc_id>")
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
