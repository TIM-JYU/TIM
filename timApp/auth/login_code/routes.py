from flask import Response

from timApp.auth.accesshelper import get_doc_or_abort
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

login_code = TypedBlueprint("login_code", __name__, url_prefix="/login_code")


@login_code.get("/group/<str: groupname>")
def get_group_by_name(groupname: str) -> Response:
    """
    :return: UserGroup matching the provided group name
    """
    group = UserGroup.get_by_name(groupname)
    group_doc = get_doc_or_abort(group.admin_doc.doc_id)
    return json_response({"id": group.id, "name": group.name, "path": group_doc.path})
