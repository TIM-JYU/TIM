"""
Helper functions for managing user sessions.
"""

from flask import has_request_context, session, current_app
from sqlalchemy import func, select, update

from timApp.auth.session.model import UserSession
from timApp.auth.sessioninfo import get_current_user_object
from timApp.item.item import Item
from timApp.tim_app import app
from timApp.timdb.sqa import db, run_sql
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


def _get_active_session_count(user: User) -> int:
    return db.session.scalar(
        select(func.count(UserSession.session_id)).filter(
            (UserSession.user == user) & ~UserSession.expired
        )
    )


def current_session_id() -> str | None:
    """
    Get the current session ID of the current user.

    .. note:: The function must be called from within a request context.

    :return: Session ID or None if not logged in or the user is missing session info.
    """
    return session.get("session_id")


def is_allowed_document(path: str) -> bool:
    """
    Check if the given path is allowed to be accessed despite the session block.

    :param path: Path to check.
    :return: True if the path is allowed, False otherwise.
    """

    paths = current_app.config.get("SESSION_BLOCK_IGNORE_DOCUMENTS", [])
    return path in paths


def expire_user_session(user: User, session_id: str | None) -> None:
    """
    Expire the given session for the given user.

    Users with expired sessions cannot access documents without re-verifying their session.

    :param user: User to expire the session for.
    :param session_id: Session ID to expire.
    """

    if not _save_sessions() or not session_id:
        return
    sess = (
        run_sql(select(UserSession).filter_by(user=user, session_id=session_id))
        .scalars()
        .first()
    )
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
    """
    Add a new session for the given user.

    :param user: User to add the session for.
    :param session_id: Session ID to add.
    :param origin: Origin of the session.
                   Origin may contain any information to identify the origin of the session (user agent, endpoint, etc).
    """
    if not _save_sessions():
        return

    expired_at_time = None
    max_sessions = _max_concurrent_sessions()
    if max_sessions and _get_active_session_count(user) >= max_sessions:
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

    ...


def has_valid_session(user: User | None = None) -> bool:
    """
    Check if the user has a valid session.

    A user is considered to have a valid session if:

    0. Session tracking is enabled (:ref:`timApp.defaultconfig.SESSIONS_ENABLE`).
    1. User is an admin.
    2. The user has a session that is not expired (doesn't have the logout date set).

    :param user: User to check for session validity. If None, the current user is used.
    :return: True if the user has a valid session, False otherwise.
    """

    from timApp.auth.sessioninfo import get_current_user_id
    from timApp.auth.accesshelper import verify_admin

    user = user if user else get_current_user_object()

    if (
        not has_request_context()
        or not _save_sessions()
        # Only check session for current user since the session ID is only valid for them
        or get_current_user_id() == get_anon_user_id()
        or get_current_user_id() != user.id
        or verify_admin(require=False, user=user)
    ):
        return True

    session_id = current_session_id()
    if not session_id:
        return False

    current_session = (
        run_sql(
            select(UserSession.session_id)
            .filter(
                (UserSession.user == user)
                & (UserSession.session_id == session_id)
                & ~UserSession.expired
            )
            .limit(1)
        )
        .scalars()
        .first()
    )

    return current_session is not None


def verify_session_for(username: str, session_id: str | None = None) -> None:
    """
    Verify that the session is valid for the user and expire all other sessions.

    :param username: Username of the user to verify the session for.
    :param session_id: If specified, verify the specific session ID. If None, verify the latest added session.
    """
    user_subquery = select(User.id).filter(User.name == username)
    stmt_base = (
        update(UserSession)
        .where(UserSession.user_id.in_(user_subquery))
        .execution_options(synchronize_session=False)
    )

    if session_id:
        stmt_expire = stmt_base.where(UserSession.session_id != session_id)
        stmt_verify = stmt_base.where(UserSession.session_id == session_id)
    else:
        # Get the latest session
        subquery = (
            select(UserSession.session_id)
            .filter(UserSession.user_id.in_(user_subquery))
            .order_by(UserSession.logged_in_at.desc())
            .limit(1)
        )
        stmt_expire = stmt_base.where(UserSession.session_id.notin_(subquery))
        stmt_verify = stmt_base.where(UserSession.session_id.in_(subquery))

    # Only expire active sessions
    stmt_expire = stmt_expire.where(UserSession.expired == False)

    run_sql(stmt_expire.values({"expired_at": get_current_time()}))
    run_sql(stmt_verify.values({"expired_at": None}))


def invalidate_sessions_for(username: str, session_id: str | None = None) -> None:
    """
    Invalidate the session for the user.

    :param username: Username of the user to invalidate the session for.
    :param session_id: If specified, invalidate the specific session ID. If None, invalidate all sessions.
    """
    user_subquery = select(User.id).filter(User.name == username)
    stmt_invalidate = (
        update(UserSession)
        .filter(UserSession.user_id.in_(user_subquery))
        .values({"expired_at": get_current_time()})
        .execution_options(synchronize_session=False)
    )
    if session_id:
        stmt_invalidate = stmt_invalidate.filter(UserSession.session_id == session_id)

    run_sql(stmt_invalidate)


def distribute_session_verification(
    action: str, username: str, session_id: str | None, targets: list[str]
) -> list[str]:
    """
    Distribute a session verification operation to the specified targets.

    .. note:: The function requires right distribution to be enabled.
              Session verification is distributed and verified with :ref:`timApp.defaultconfig.DIST_RIGHTS_SEND_SECRET`
              and :ref:`timApp.defaultconfig.DIST_RIGHTS_RECEIVE_SECRET` configuration options.

    :param action: Action to perform. Usually either "verify" or "invalidate".
    :param username: Username of the user to verify the session for.
    :param session_id: If specified, verify the specific session ID.
    :param targets: List of targets from :ref:`timApp.defaultconfig.DIST_RIGHTS_HOSTS`
                    to distribute the session verification to.
    :return: List of errors that occurred during distribution.
    """
    if not targets:
        return []

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
    """
    Checks if the user's current session has access to the given item.

    :param i: Item or block to check access for.
    :param user: User to check access for.
    :return: True if the user has access and valid session, False otherwise.
    """
    return has_valid_session(user) or (
        isinstance(i, Item) and is_allowed_document(i.path)
    )
