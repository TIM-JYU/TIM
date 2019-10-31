import os
import shutil
from os.path import isfile
from typing import List

from timApp.document.docentry import DocEntry
from timApp.document.translation.translation import Translation
from timApp.folder.folder import Folder
from timApp.item.block import Block, BlockType
from timApp.tim_app import app
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


def fix_orphans_without_docentry():
    """Finds all documents that do not have a DocEntry and creates a DocEntry for them under 'orphans' directory."""
    with app.test_request_context():
        orphan_folder_title = 'orphans'
        f = Folder.create('orphans', UserGroup.get_admin_group())
        orphans: List[Block] = Block.query.filter(
            (Block.type_id == 0) &
            Block.id.notin_(DocEntry.query.with_entities(DocEntry.id)) &
            Block.id.notin_(Translation.query.with_entities(Translation.doc_id))
        ).all()

        for o in orphans:
            print(f'Adding a DocEntry for document with id {o.id}')
            # noinspection PyArgumentList
            d = DocEntry(id=o.id, name=f'{f.path}/orphan_{o.id}', public=True)
            db.session.add(d)
        db.session.commit()
        print(f"Fixed {len(orphans)} documents without a DocEntry. They are in '{orphan_folder_title}' folder.")


def move_docs_without_block():
    """Moves all documents from tim_files/docs to tim_files/orphans that don't have a Block entry in database."""
    with app.test_request_context():
        files_root = get_files_path()
        docs_folder = os.path.join(files_root, 'docs')
        pars_folder = os.path.join(files_root, 'pars')
        doc_folders = [f for f in os.listdir(docs_folder) if not isfile(f)]
        existing_blocks = set(str(i) for i, in Block.query.filter_by(type_id=BlockType.Document.value).with_entities(Block.id).all())
        docs_orphans = os.path.join(files_root, 'orphans', 'docs')
        pars_orphans = os.path.join(files_root, 'orphans', 'pars')
        os.makedirs(docs_orphans, exist_ok=True)
        os.makedirs(pars_orphans, exist_ok=True)
        fixed = 0
        for f in doc_folders:
            if f not in existing_blocks:
                fixed += 1
                doc_dir = os.path.join(docs_folder, f)
                print(f'Moving {doc_dir} to orphans')
                shutil.move(doc_dir, docs_orphans)
                par_dir = os.path.join(pars_folder, f)
                if os.path.exists(par_dir):
                    print(f'Moving {par_dir} to orphans')
                    shutil.move(par_dir, pars_orphans)
        print(f"Found {fixed} documents without a Block. They are in tim_files/orphans.")


if __name__ == '__main__':
    fix_orphans_without_docentry()
    move_docs_without_block()
