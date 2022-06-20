"""
Utilities for access control.
"""

from flask import session

from timApp.auth.accesstype import AccessType
from timApp.document.caching import clear_doc_cache


def get_locked_access_type() -> AccessType | None:
    """
    Get the locked access type of the current user.

    :return: Access type or None if access type is not locked.
    """
    locked_access_value = session.get("locked_access_type", None)
    if locked_access_value is None:
        return None
    try:
        return AccessType(locked_access_value)
    except ValueError:
        session.pop("locked_access_type", None)
        return None


def set_locked_access_type(access_type: AccessType | None) -> None:
    """
    Set the locked access type of the current user.

    :param access_type: Access type to lock to. If None, resets access type.
    """
    from timApp.auth.sessioninfo import get_current_user_object

    if access_type is None:
        session.pop("locked_access_type", None)
    else:
        session["locked_access_type"] = access_type.value

    clear_doc_cache(None, get_current_user_object())


def get_locked_active_groups() -> set[int] | None:
    """
    Get the locked active groups of the current user.

    :return: Set of group IDs or None if active groups are not locked.
    """
    res = session.get("locked_active_groups", None)
    return set(res) if res is not None else None


def set_locked_active_groups(active_groups: set[int] | None) -> None:
    """
    Set the locked active groups of the current user.

    :param active_groups: Set of active group IDs to lock to the user to. If None, resets active groups.
    """
    from timApp.auth.sessioninfo import get_current_user_object

    if active_groups is None:
        session.pop("locked_active_groups", None)
    else:
        session["locked_active_groups"] = list(active_groups)

    clear_doc_cache(None, get_current_user_object())
