"""
Routes for managing user sessions.
"""

from _csv import QUOTE_ALL
from dataclasses import field
from enum import Enum
from typing import Any

from flask import Response
from sqlalchemy import update, select

from timApp.auth.accesshelper import verify_logged_in, verify_admin
from timApp.auth.session.model import UserSession
from timApp.auth.session.util import (
    current_session_id,
    has_valid_session,
    verify_session_for,
    invalidate_sessions_for,
)
from timApp.tim_app import csrf
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import json_response, csv_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.secret import check_secret
from timApp.util.utils import get_current_time

user_sessions = TypedBlueprint("user_sessions", __name__, url_prefix="/user/sessions")


@user_sessions.get("/current")
def get_current_session() -> Response:
    """
    Get the information about the current session.

    :return: A JSON response with the session ID and validity.
    """
    verify_logged_in()
    return json_response(
        {"sessionId": current_session_id(), "valid": has_valid_session()}
    )


class SessionStateFilterOptions(Enum):
    """
    Options for filtering the sessions by state.
    """

    ALL = "all"
    """Allow any session state."""

    EXPIRED = "expired"
    """Only allow expired sessions."""

    ACTIVE = "active"
    """Only allow active sessions."""


class ExportFormatOptions(Enum):
    """
    Options for session export format.
    """

    CSV = "csv"
    """Export sessions as CSV."""

    JSON = "json"
    """Export sessions as JSON."""


@user_sessions.get("/all")
def get_all_sessions(
    state: SessionStateFilterOptions = field(
        default=SessionStateFilterOptions.ALL,
        metadata={"by_value": True},
    ),
    user: str | None = None,
    export_format: ExportFormatOptions = field(
        default=ExportFormatOptions.CSV,
        metadata={
            "by_value": True,
            "data_key": "format",
        },
    ),
) -> Response:
    """
    Get all sessions information.

    .. note:: This endpoint is only accessible to administrators.

    :param state: Session state to filter by.
    :param user: Username to filter by.
    :param export_format: Export format to use.
    :return: User sessions information in the specified format.
    """
    verify_admin()
    stmt = select(UserSession)

    match state:
        case SessionStateFilterOptions.ACTIVE:
            stmt = stmt.filter(UserSession.expired == False)
        case SessionStateFilterOptions.EXPIRED:
            stmt = stmt.filter(UserSession.expired == True)
        case _:
            pass

    if user:
        stmt = stmt.join(User).filter(User.name == user)

    match export_format:
        case ExportFormatOptions.JSON:
            return json_response(db.session.execute(stmt).scalars().all())
        case ExportFormatOptions.CSV:
            data: list[list[Any]] = [
                ["user", "session_id", "origin", "logged_in_at", "expired_at"]
            ]
            for s in db.session.execute(stmt).scalars().all():  # type: UserSession
                data.append(
                    [
                        s.user.name,
                        s.session_id,
                        s.origin,
                        s.logged_in_at,
                        s.expired_at,
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
    """
    Validate a session.

    A validated session will be unexpired while all other sessions will be expired.

    .. note:: This endpoint is only accessible to administrators.

    :param user: Username to validate.
    :param session_id: Session ID to validate. If not specified, the latest session will be validated.
    :return: OK response.
    """
    verify_admin()
    verify_session_for(user, session_id)
    db.session.commit()
    return ok_response()


@user_sessions.get("/<user>/invalidate")
def invalidate_session(user: str, session_id: str | None = None) -> Response:
    """
    Invalidate a session.

    .. note:: This endpoint is only accessible to administrators.

    :param user: Username to invalidate.
    :param session_id: Session ID to invalidate. If not specified, all sessions will be invalidated.
    :return: OK response.
    """
    verify_admin()
    invalidate_sessions_for(user, session_id)
    db.session.commit()
    return ok_response()


@user_sessions.post("/verify")
@csrf.exempt
def validate_remote_session(
    username: str, session_id: str | None = None, secret: str | None = None
) -> Response:
    """
    Validate a session.

    A validated session will be unexpired while all other sessions will be expired.

    :param username: Username to validate.
    :param session_id: Session ID to validate. If not specified, the latest session will be validated.
    :param secret: Right distribution secret (:ref:`timApp.defaulconfig.DIST_RIGHTS_RECEIVE_SECRET`).
                   If not specified, the caller must be an administrator.
    :return: OK response if session was validated.
    """

    if not secret:
        verify_admin()
    else:
        check_secret(secret, "DIST_RIGHTS_RECEIVE_SECRET")
    verify_session_for(username, session_id)
    db.session.commit()
    return ok_response()


@user_sessions.get("/verifyAll")
def validate_all() -> Response:
    """
    Validate all latest sessions.
    """
    verify_admin()

    all_usersnames: list[tuple[str]] = (
        db.session.execute(
            select(User.name).join(UserSession).distinct(UserSession.user_id)
        )
        .scalars()
        .all()
    )
    for (user,) in all_usersnames:
        verify_session_for(user)

    db.session.commit()
    return ok_response()


@user_sessions.get("/invalidateAll")
def invalidate_all() -> Response:
    """
    Invalidate all sessions.
    """
    verify_admin()

    db.session.execute(
        update(UserSession)
        .where(~UserSession.expired)
        .values({"expired_at": get_current_time()})
    )
    db.session.commit()
    return ok_response()


@user_sessions.post("/invalidate")
@csrf.exempt
def invalidate_remote_session(
    username: str, session_id: str | None = None, secret: str | None = None
) -> Response:
    """
    Invalidate a session.

    A validated session will be unexpired while all other sessions will be expired.

    :param username: Username to invalidate.
    :param session_id: Session ID to invalidate. If not specified, all sessions will be invalidated.
    :param secret: Right distribution secret (:ref:`timApp.defaulconfig.DIST_RIGHTS_RECEIVE_SECRET`).
                   If not specified, the caller must be an administrator.
    :return: OK response if session was invalidated.
    """

    if not secret:
        verify_admin()
    else:
        check_secret(secret, "DIST_RIGHTS_RECEIVE_SECRET")
    invalidate_sessions_for(username, session_id)
    db.session.commit()
    return ok_response()
