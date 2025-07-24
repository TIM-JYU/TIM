from flask import Response

from timApp.auth.sessioninfo import get_current_user_object
from timApp.timdb.sqa import db
from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import get_current_time

tos_accepted = TypedBlueprint("tos_accepted", __name__, url_prefix="")


@tos_accepted.post("/tosagree")
def tosagree() -> Response:
    curr_user = get_current_user_object()

    if curr_user.is_real_user:
        now = get_current_time()
        if curr_user.tos_accepted_at != now:
            curr_user.tos_accepted_at = now
            db.session.commit()
    return ok_response()
