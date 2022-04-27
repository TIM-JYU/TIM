from flask import has_request_context, session, current_app
from sqlalchemy import func

from timApp.auth.session.model import UserSession
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.userutils import get_anon_user_id
from timApp.util.logger import log_info, log_warning


def _save_sessions() -> bool:
    return current_app.config["SESSIONS_ENABLE"]


def _expire_on_logout() -> bool:
    return current_app.config["SESSIONS_EXPIRE_ON_LOGOUT"]


def _max_concurrent_sessions() -> int | None:
    return current_app.config["SESSIONS_MAX_CONCURRENT_SESSIONS_PER_DOCUMENT"]


def expire_user_session(user: User, session_id: str | None) -> None:
    if not _save_sessions() or not _expire_on_logout() or not session_id:
        return
    if sess := user.active_sessions.get(session_id):
        log_info(
            f"SESSION: {user.name} logged out (has {len(user.active_sessions) - 1} active sessions left)"
        )
        sess.expire()
    else:
        log_warning(
            f"SESSION: {user.name} was logged out but did not have session info"
        )


def add_user_session(user: User, session_id: str, origin: str) -> None:
    if not _save_sessions():
        return
    user.active_sessions[session_id] = UserSession(
        session_id=session_id,
        origin=origin,
        logged_out_at=None,
    )

    log_info(f"SESSION: {user.name} ({len(user.active_sessions)} active sessions)")


def has_valid_session(user: User) -> bool:
    from timApp.auth.sessioninfo import get_current_user_id

    if not _save_sessions():
        return True
    if not has_request_context():
        return True
    # Only check session for current user since the session ID is only valid for them
    if get_current_user_id() == get_anon_user_id() or get_current_user_id() != user.id:
        return True

    session_id = session.get("session_id")
    if not session_id:
        return False

    current_session = (
        db.session.query(UserSession.session_id)
        .filter(
            (UserSession.user == user)
            & (UserSession.session_id == session_id)
            & ~UserSession.expired
        )
        .first()
    )

    if not current_session:
        return False

    active_session_count = (
        db.session.query(func.count(UserSession.session_id))
        .filter((UserSession.user == user) & ~UserSession.expired)
        .scalar()
    )

    if (
        _max_concurrent_sessions() is not None
        and active_session_count > _max_concurrent_sessions()
    ):
        return False

    return True
