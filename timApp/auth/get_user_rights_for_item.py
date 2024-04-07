from __future__ import annotations

from typing import TYPE_CHECKING, TypedDict

if TYPE_CHECKING:
    from timApp.item.item import ItemBase
    from timApp.user.user import User


# TODO convert this to a dataclass
class UserItemRights(TypedDict):
    editable: bool
    can_mark_as_read: bool
    can_comment: bool
    copy: bool
    browse_own_answers: bool
    teacher: bool
    see_answers: bool
    manage: bool
    owner: bool
    restricted_mode: bool


def get_user_rights_for_item(
    d: ItemBase, u: User, allow_duration: bool = False
) -> UserItemRights:
    editable = u.has_edit_access(
        d,
        duration=allow_duration,
    )
    viewable = u.has_view_access(
        d,
        duration=allow_duration,
    )
    copyable = u.has_copy_access(
        d,
        duration=allow_duration,
    )
    teacher = u.has_teacher_access(
        d,
        duration=allow_duration,
    )
    see_answers = u.has_seeanswers_access(
        d,
        duration=allow_duration,
    )
    manage = u.has_manage_access(
        d,
        duration=allow_duration,
    )
    owner = u.has_ownership(d)
    is_restricted_mode = any(
        bool(a.restricted) if a else False
        for a in (editable, viewable, copyable, teacher, see_answers, manage, owner)
    )
    return {
        "editable": bool(editable),
        "can_mark_as_read": bool(u.logged_in and viewable),
        "can_comment": bool(u.logged_in and viewable),
        "copy": bool(u.logged_in and copyable),
        "browse_own_answers": u.logged_in,
        "teacher": bool(teacher),
        "see_answers": bool(see_answers),
        "manage": bool(manage),
        "owner": bool(owner),
        "restricted_mode": is_restricted_mode,
    }
