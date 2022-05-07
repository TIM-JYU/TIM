from flask import has_request_context, session, current_app
from sqlalchemy import func

from timApp.auth.session.model import UserSession
from timApp.auth.sessioninfo import get_current_user_object
from timApp.item.item import Item
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User, ItemOrBlock
from timApp.user.userutils import get_anon_user_id
from timApp.util.logger import log_info, log_warning
from timApp.util.secret import get_secret_or_abort
from timApp.util.utils import get_current_time
from tim_common.vendor.requests_futures import FuturesSession


def _save_sessions() -> bool:
    return current_app.config["SESSIONS_ENABLE"]


def _max_concurrent_sessions() -> int | None:
    return current_app.config["SESSIONS_MAX_CONCURRENT_SESSIONS_PER_USER"]


def current_session_id() -> str | None:
    return session.get("session_id")


def is_allowed_document(path: str) -> bool:
    paths = current_app.config.get("SESSION_BLOCK_IGNORE_DOCUMENTS", [])
    return path in paths


def _get_active_session_count(user: User) -> int:
    return (
        db.session.query(func.count(UserSession.session_id))
        .filter((UserSession.user == user) & ~UserSession.expired)
        .scalar()
    )


def expire_user_session(user: User, session_id: str | None) -> None:
    if not _save_sessions() or not session_id:
        return
    sess = UserSession.query.filter_by(user=user, session_id=session_id).first()
    if sess:
        log_info(
            f"SESSION: {user.name} logged out (expired={sess.expired}, active={_get_active_session_count(user) - 1})"
        )
        sess.expire()
    else:
        log_warning(
            f"SESSION: {user.name} was logged out but did not have session info"
        )


def add_user_session(user: User, session_id: str, origin: str) -> None:
    if not _save_sessions():
        return

    expired_at_time = None
    if (
        _max_concurrent_sessions()
        and _get_active_session_count(user) >= _max_concurrent_sessions()
    ):
        expired_at_time = get_current_time()

    us = UserSession(
        user=user,
        session_id=session_id,
        origin=origin,
        expired_at=expired_at_time,
    )
    db.session.add(us)

    log_info(
        f"SESSION: {user.name} logged in (expired={us.expired}, active={len(user.active_sessions)})"
    )


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
    1. The user has a session that is not expired (doesn't have the logout date set).

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

    return current_session is not None


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

    q_expire.update({"expired_at": get_current_time()}, synchronize_session=False)
    q_verify.update({"expired_at": None}, synchronize_session=False)


def invalidate_sessions_for(username: str, session_id: str | None = None) -> None:
    """
    Invalidate the session for the user.

    :param username: Username of the user to invalidate the session for.
    :param session_id: If specified, invalidate the specific session ID. If None, invalidate all sessions.
    """
    user_subquery = db.session.query(User.id).filter(User.name == username).subquery()
    q_invalidate = UserSession.query.filter(UserSession.user_id.in_(user_subquery))

    if session_id:
        q_invalidate = q_invalidate.filter(UserSession.session_id == session_id)

    q_invalidate.update({"expired_at": get_current_time()}, synchronize_session=False)


def distribute_session_verification(
    action: str, username: str, session_id: str | None, targets: list[str]
) -> list[str]:
    hosts = set()
    dist_rights_send_secret = get_secret_or_abort("DIST_RIGHTS_SEND_SECRET")
    for target in targets:
        h = app.config["DIST_RIGHTS_HOSTS"].get(target, {}).get("hosts", [])
        hosts.update(h)

    if not hosts:
        return []

    r_session = FuturesSession()
    futures = []
    for host in hosts:
        r = r_session.post(
            f"{host}/user/sessions/{action}",
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


def session_has_access(i: ItemOrBlock, user: User | None = None) -> bool:
    return has_valid_session(user) or (
        isinstance(i, Item) and is_allowed_document(i.path)
    )
