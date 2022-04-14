from timApp.auth.session.model import UserSession
from timApp.tim_app import app
from timApp.user.user import User
from timApp.util.logger import log_info, log_warning


def _save_sessions() -> bool:
    return app.config["SESSIONS_ENABLE"]


def expire_user_session(user: User, session_id: str | None) -> None:
    if not _save_sessions() or not session_id:
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
