from flask import Response, current_app
from datetime import datetime

from timApp.auth.sessioninfo import get_current_user_object
from timApp.item.item import Item
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


def get_tos_date() -> datetime | None:
    """Returns the date when tos document was last modified."""
    tos_path = current_app.config["TERMS_OF_SERVICE_DOC"]
    if tos_path:
        tos_doc = Item.find_by_path(tos_path)
        if tos_doc:
            return tos_doc.last_modified
        else:
            return None
    else:
        return None
