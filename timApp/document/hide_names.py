from typing import Optional

from flask import session

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.user.user import User


def hide_names_in_teacher(d: DocInfo, context_user: Optional[User] = None) -> bool:
    """Determines whether user names should be hidden.

    :param d: The document we're viewing.
    :param context_user: The user whose data we are inspecting. If same as currently logged-in user, we don't have to
    force hiding.
    """
    u = get_current_user_object()
    force_hide = False
    if context_user and context_user.id == u.id:
        pass
    else:
        force_hide = not u.has_teacher_access(d)
    return is_hide_names() or force_hide


def is_hide_names() -> bool:
    return session.get('hide_names', False)
