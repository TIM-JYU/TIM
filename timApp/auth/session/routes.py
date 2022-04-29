"""
Routes for managing user sessions
"""
from flask import Response

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.session.util import current_session_id, has_valid_session
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

user_sessions = TypedBlueprint("user_sessions", __name__, url_prefix="/user/sessions")


@user_sessions.get("/current")
def get_current_session() -> Response:
    verify_logged_in()
    return json_response(
        {"sessionId": current_session_id(), "valid": has_valid_session()}
    )
