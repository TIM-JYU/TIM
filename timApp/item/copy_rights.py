from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.item.item import Item
from timApp.user.user import User
from timApp.util.utils import get_current_time


def copy_rights(source: Item, dest: Item, new_owner: User):
    for a in source.block.accesses:  # type: BlockAccess
        # We don't want to copy owner or copy rights.
        if a.access_type in (AccessType.owner, AccessType.copy):
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
        if b not in dest.block.accesses:
            dest.block.accesses.append(b)
    g = new_owner.get_personal_group()
    o_a = BlockAccess(
        block_id=dest.block.id,
        usergroup_id=g.id,
        type=AccessType.owner.value,
        accessible_from=get_current_time(),
    )
    if o_a not in dest.block.accesses:
        dest.block.accesses.append(o_a)
