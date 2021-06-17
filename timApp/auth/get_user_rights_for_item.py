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


def get_user_rights_for_item(d: ItemBase, u: User) -> UserItemRights:
    return {
        'editable': bool(u.has_edit_access(d)),
        'can_mark_as_read': bool(u.logged_in and u.has_view_access(d)),
        'can_comment': bool(u.logged_in and u.has_view_access(d)),
        'copy': bool(u.logged_in and u.has_copy_access(d)),
        'browse_own_answers': u.logged_in,
        'teacher': bool(u.has_teacher_access(d)),
        'see_answers': bool(u.has_seeanswers_access(d)),
        'manage': bool(u.has_manage_access(d)),
        'owner': bool(u.has_ownership(d))
    }
