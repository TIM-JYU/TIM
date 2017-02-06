"""Finds all documents that do not have a DocEntry and creates a DocEntry for them under 'orphans' directory."""
from typing import List

from dbaccess import get_timdb
from tim_app import app
from timdb.models.block import Block
from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timdb.models.translation import Translation
from timdb.tim_models import db


def fix_orphans():
    with app.test_request_context():
        timdb = get_timdb()
        orphan_folder_title = 'orphans'
        f = Folder.create('orphans', timdb.users.get_admin_group_id())
        orphans = Block.query.filter(
            (Block.type_id == 0) &
            Block.id.notin_(DocEntry.query.with_entities(DocEntry.id)) &
            Block.id.notin_(Translation.query.with_entities(Translation.doc_id))
        ).all()  # type: List[Block]

        for o in orphans:
            print('Fixing document with id {}'.format(o.id))
            d = DocEntry(id=o.id, name='{}/orphan_{}'.format(f.path, o.id), public=True)
            db.session.add(d)
        db.session.commit()
        print("Fixed {} orphaned documents. They are in '{}' folder.".format(len(orphans), orphan_folder_title))


if __name__ == '__main__':
    fix_orphans()
