from flask import has_request_context, session, current_app
from sqlalchemy import func

from timApp.auth.session.model import UserSession
from timApp.auth.sessioninfo import get_current_user_object
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.userutils import get_anon_user_id
from timApp.util.logger import log_info, log_warning
from timApp.util.secret import get_secret_or_abort
from timApp.util.utils import get_current_time
from tim_common.vendor.requests_futures import FuturesSession


def _save_sessions() -> bool:
    return current_app.config["SESSIONS_ENABLE"]


def _expire_on_logout() -> bool:
    return current_app.config["SESSIONS_EXPIRE_ON_LOGOUT"]


def _max_concurrent_sessions() -> int | None:
    return current_app.config["SESSIONS_MAX_CONCURRENT_SESSIONS_PER_DOCUMENT"]


def current_session_id() -> str | None:
    return session.get("session_id")


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


class SessionExpired(Exception):
    """
    Raised when current user's session is expired.
    """

    pass


def has_valid_session(user: User | None = None) -> bool:
    """
    Check if the user has a valid session.

    A user is considered to have a valid session if:

    0. Session tracking is enabled (:ref:`timApp.defaultconfig.SESSIONS_ENABLE`).
    1. The user has a session that is not expired (doesn't have the logout date set)
    2. The number of session that are not expired is less than the maximum number
       of concurrent sessions (:ref:`timApp.defaultconfig.SESSIONS_MAX_CONCURRENT_SESSIONS_PER_DOCUMENT`).

    :param user: User to check for session validity. If None, the current user is used.
    :return: True if the user has a valid session, False otherwise.
    """

    from timApp.auth.sessioninfo import get_current_user_id

    user = user if user else get_current_user_object()

    if (
        not has_request_context()
        or not _save_sessions()
        # Only check session for current user since the session ID is only valid for them
        or get_current_user_id() == get_anon_user_id()
        or get_current_user_id() != user.id
    ):
        return True

    session_id = current_session_id()
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


def verify_session_for(username: str, session_id: str | None = None) -> None:
    """
    Verify that the session is valid for the user and expire all other sessions.

    :param username: Username of the user to verify the session for.
    :param session_id: If specified, verify the specific session ID. If None, verify the latest added session.
    """
    user_subquery = db.session.query(User.id).filter(User.name == username).subquery()
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


def distribute_session_verification(
    username: str, session_id: str | None, targets: list[str]
) -> list[str]:
    hosts = set()
    dist_rights_send_secret = get_secret_or_abort("DIST_RIGHTS_SEND_SECRET")
    for target in targets:
        h = app.config["DIST_RIGHTS_HOSTS"].get(target, {}).get("hosts", [])
        hosts.update(h)

    r_session = FuturesSession()
    futures = []
    for host in hosts:
        r = r_session.post(
            f"{host}/user/sessions/verify",
            json={
                "username": username,
                "session_id": session_id,
                "secret": dist_rights_send_secret,
            },
            headers={"Content-Type": "application/json"},
            timeout=10,
        )
        futures.append(r)

    errors = []
    for r, host in zip(futures, hosts):
        try:
            res = r.result()
        except Exception as e:
            errors.append(f"{host}: {e}")
        else:
            if res.status_code != 200:
                errors.append(f"{host} ({res.status_code}): {res.text}")

    return errors
