from typing import Optional

from timdb.models.block import Block
from timdb.tim_models import db


def insert_block(description: Optional[str], owner_group_id: int, block_type: int, commit: bool = True) -> int:
    """Inserts a block to database.

    :param commit: Whether to commit the change.
    :param description: The name (description) of the block.
    :param owner_group_id: The owner group of the block.
    :param block_type: The type of the block.
    :returns: The id of the block.
    """
    b = Block(description=description, type_id=block_type, usergroup_id=owner_group_id)
    db.session.add(b)
    db.session.flush()
    block_id = b.id
    assert block_id != 0
    if commit:
        db.session.commit()
    return block_id


def copy_default_rights(item_id: int, item_type, commit=True):
    # TODO: Should not need to import anything from routes
    from routes.dbaccess import get_timdb
    timdb = get_timdb()
    default_rights = []
    folder = Block.query.get(item_id).get_parent()
    while folder is not None:
        default_rights += timdb.users.get_default_rights_holders(folder.id, item_type)
        folder = folder.get_parent()
    for d in default_rights:
        timdb.users.grant_access(d['gid'], item_id, d['access_name'], commit=commit)
