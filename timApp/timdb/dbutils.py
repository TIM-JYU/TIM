from datetime import datetime, timezone
from typing import Optional

from timdb.accesstype import AccessType
from timdb.models.block import Block
from timdb.tim_models import db, BlockAccess


def insert_block(description: Optional[str], owner_group_id: Optional[int], block_type: int,
                 commit: bool = True) -> Block:
    """Inserts a block to database.

    :param commit: Whether to commit the change.
    :param description: The name (description) of the block.
    :param owner_group_id: The owner group of the block.
    :param block_type: The type of the block.
    :returns: The id of the block.

    """
    b = Block(description=description, type_id=block_type)
    if owner_group_id is not None:
        access = BlockAccess(block=b,
                             usergroup_id=owner_group_id,
                             type=AccessType.owner.value,
                             accessible_from=datetime.now(tz=timezone.utc))
        db.session.add(access)
    db.session.add(b)
    db.session.flush()
    assert b.id != 0
    if commit:
        db.session.commit()
    return b


def copy_default_rights(item_id: int, item_type, commit=True):
    # TODO: Should not need to import anything from routes
    from dbaccess import get_timdb
    from timdb.userutils import grant_access
    timdb = get_timdb()
    default_rights = []
    folder = Block.query.get(item_id).parent
    while folder is not None:
        default_rights += timdb.users.get_default_rights_holders(folder.id, item_type)
        folder = folder.parent
    for d in default_rights:
        grant_access(d['gid'],
                     item_id,
                     d['access_name'],
                     commit=commit,
                     accessible_from=d['accessible_from'],
                     accessible_to=d['accessible_to'],
                     duration_from=d['duration_from'],
                     duration_to=d['duration_to'],
                     duration=d['duration'])
