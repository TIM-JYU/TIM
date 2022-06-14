from dataclasses import field

from flask import Response

from timApp.auth.access.util import set_locked_access_type
from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.accesstype import AccessType
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

access = TypedBlueprint("access", __name__, url_prefix="/access")


@access.post("lock")
def lock_access(
    access_type: AccessType | None = field(metadata={"by_value": True}),
) -> Response:
    verify_logged_in()
    set_locked_access_type(access_type)
    return ok_response()
