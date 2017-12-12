from datetime import datetime, timezone
from typing import Optional

from timApp.timdb.accesstype import AccessType
from timApp.timdb.blocktypes import BlockType
from timApp.timdb.models.block import Block
from timApp.timdb.tim_models import db, BlockAccess


def insert_block(description: Optional[str], owner_group_id: Optional[int], block_type: int) -> Block:
    """Inserts a block to database.

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
    return b


def copy_default_rights(item_id: int, item_type: BlockType, commit=True):
    # TODO: Should not need to import anything from routes
    from timApp.dbaccess import get_timdb
    from timApp.timdb.userutils import grant_access
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
