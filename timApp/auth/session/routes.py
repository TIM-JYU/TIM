"""
Routes for managing user sessions
"""
from _csv import QUOTE_ALL
from dataclasses import field
from enum import Enum

from flask import Response

from timApp.auth.accesshelper import verify_logged_in, verify_admin
from timApp.auth.session.model import UserSession
from timApp.auth.session.util import current_session_id, has_valid_session
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import json_response, csv_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import get_current_time

user_sessions = TypedBlueprint("user_sessions", __name__, url_prefix="/user/sessions")


@user_sessions.get("/current")
def get_current_session() -> Response:
    verify_logged_in()
    return json_response(
        {"sessionId": current_session_id(), "valid": has_valid_session()}
    )


class ExpiredFilterOptions(Enum):
    ALL = "all"
    EXPIRED = "expired"
    ACTIVE = "active"


class ExportFormatOptions(Enum):
    CSV = "csv"
    JSON = "json"


@user_sessions.get("/all")
def get_all_sessions(
    expired: ExpiredFilterOptions = field(
        default=ExpiredFilterOptions.ALL,
        metadata={"by_value": True},
    ),
    user: str | None = None,
    export_format: ExportFormatOptions = field(
        default=ExportFormatOptions.JSON,
        metadata={
            "by_value": True,
            "data_key": "format",
        },
    ),
) -> Response:
    verify_admin()
    q = UserSession.query

    match expired:
        case ExpiredFilterOptions.ACTIVE:
            q = q.filter(UserSession.expired == False)
        case ExpiredFilterOptions.EXPIRED:
            q = q.filter(UserSession.expired == True)
        case _:
            pass

    if user:
        q = q.join(User).filter(User.name == user)

    match export_format:
        case ExportFormatOptions.JSON:
            return json_response(q.all())
        case ExportFormatOptions.CSV:
            data = [["user", "session_id", "origin", "logged_in", "logged_out"]]
            for s in q.all():  # type: UserSession
                data.append(
                    [
                        s.user.name,
                        s.session_id,
                        s.origin,
                        s.logged_in_at,
                        s.logged_out_at,
                    ]
                )
            return csv_response(
                data,
                quoting=QUOTE_ALL,
                pad_spaces=True,
            )
    raise RouteException("Invalid export format")


@user_sessions.get("/<user>/verify")
def validate_session(user: str, session_id: str | None = None) -> Response:
    verify_admin()
    user_subquery = db.session.query(User.id).filter(User.name == user).subquery()
    q_base = UserSession.query.filter(UserSession.user_id.in_(user_subquery))
    if session_id:
        q_expire = q_base.filter(UserSession.session_id != session_id)
        q_verify = q_base.filter(UserSession.session_id == session_id)
    else:
        # Get the latest active session
        subquery = (
            db.session.query(UserSession.session_id)
            .filter(UserSession.expired == False)
            .order_by(UserSession.logged_in_at.desc())
            .limit(1)
            .subquery()
        )
        q_expire = q_base.filter(UserSession.session_id.notin_(subquery))
        q_verify = q_base.filter(UserSession.session_id.in_(subquery))

    # Only expire active sessions
    q_expire = q_expire.filter(UserSession.expired == False)

    q_expire.update({"logged_out_at": get_current_time()}, synchronize_session=False)
    q_verify.update({"logged_out_at": None}, synchronize_session=False)
    db.session.commit()
    return ok_response()
