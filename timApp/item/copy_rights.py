from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.item.item import Item
from timApp.user.user import User
from timApp.util.utils import get_current_time


def copy_rights(
    source: Item,
    dest: Item,
    new_owner: User | None,
    copy_active=True,
    copy_expired=True,
) -> None:
    for key, a in source.block.accesses.items():
        # We don't want to copy owner or copy rights.
        if new_owner and a.access_type in (AccessType.owner, AccessType.copy):
            continue
        if not copy_expired and (a.expired or a.duration_expired):
            continue
        if not copy_active and (not a.expired and not a.duration_expired):
            continue
        b = BlockAccess(
            block_id=dest.block.id,
            usergroup_id=a.usergroup_id,
            type=a.type,
            accessible_from=a.accessible_from,
            accessible_to=a.accessible_to,
            duration=a.duration,
            duration_from=a.duration_from,
            duration_to=a.duration_to,
        )
        dest.block.accesses[key] = b
    if new_owner:
        g = new_owner.get_personal_group()
        o_a = BlockAccess(
            block_id=dest.block.id,
            usergroup_id=g.id,
            type=AccessType.owner.value,
            accessible_from=get_current_time(),
        )
        key = (g.id, AccessType.owner.value)
        dest.block.accesses[key] = o_a
