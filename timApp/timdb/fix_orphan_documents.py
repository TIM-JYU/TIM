"""Finds all documents that do not have a DocEntry and creates a DocEntry for them under 'orphans' directory."""
from typing import List

from timApp.tim_app import app
from timApp.timdb.models.block import Block
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.translation import Translation
from timApp.timdb.tim_models import db
from timApp.timdb.userutils import get_admin_group_id


def fix_orphans():
    with app.test_request_context():
        orphan_folder_title = 'orphans'
        f = Folder.create('orphans', get_admin_group_id())
        orphans: List[Block] = Block.query.filter(
            (Block.type_id == 0) &
            Block.id.notin_(DocEntry.query.with_entities(DocEntry.id)) &
            Block.id.notin_(Translation.query.with_entities(Translation.doc_id))
        ).all()

        for o in orphans:
            print(f'Fixing document with id {o.id}')
            # noinspection PyArgumentList
            d = DocEntry(id=o.id, name=f'{f.path}/orphan_{o.id}', public=True)
            db.session.add(d)
        db.session.commit()
        print(f"Fixed {len(orphans)} orphaned documents. They are in '{orphan_folder_title}' folder.")


if __name__ == '__main__':
    fix_orphans()
