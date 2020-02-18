from typing import List

from flask.cli import AppGroup

from timApp.item.block import Block, BlockType
from timApp.timdb.sqa import db

item_cli = AppGroup('item')


@item_cli.command('cleanup_default_rights_names')
def cleanup_default_right_doc_names():
    bs: List[Block] = Block.query.filter(
        Block.description.in_(
            [
                'templates/DefaultDocumentRights',
                'templates/DefaultFolderRights',
                '$DefaultFolderRights',
                '$DefaultDocumentRights',
            ]) & (Block.type_id == BlockType.Document.value)
    ).all()
    num_changed = len(bs)
    for b in bs:
        b.description = b.description.replace('templates/', '').replace('$', '')
    db.session.commit()
    print(f'Fixed titles of {num_changed} default rights documents.')
