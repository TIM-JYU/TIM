from __future__ import annotations

from typing import TYPE_CHECKING, Dict

if TYPE_CHECKING:
    from timApp.item.item import ItemBase
    from timApp.user.user import User


def get_user_rights_for_item(d: ItemBase, u: User) -> Dict[str, bool]:
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
