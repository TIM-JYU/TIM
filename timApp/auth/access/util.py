from flask import session

from timApp.auth.accesstype import AccessType
from timApp.document.caching import clear_doc_cache


def get_locked_access_type() -> AccessType | None:
    locked_access_value = session.get("locked_access_type", None)
    if locked_access_value is None:
        return None
    try:
        return AccessType(locked_access_value)
    except ValueError:
        session.pop("locked_access_type", None)
        return None


def set_locked_access_type(access_type: AccessType | None) -> None:
    from timApp.auth.sessioninfo import get_current_user_object

    if access_type is None:
        session.pop("locked_access_type", None)
    else:
        session["locked_access_type"] = access_type.value

    clear_doc_cache(None, get_current_user_object())
