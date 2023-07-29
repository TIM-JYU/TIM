import os
import shutil
from os.path import isfile

from sqlalchemy import select

from timApp.document.docentry import DocEntry
from timApp.document.translation.translation import Translation
from timApp.folder.folder import Folder
from timApp.item.block import Block, BlockType
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db, run_sql
from timApp.user.usergroup import UserGroup


def fix_orphans_without_docentry() -> None:
    """Finds all documents (in Block table) that do not have a DocEntry and
    creates a DocEntry for them under 'orphans' directory."""
    orphan_folder_title = "orphans"
    f = Folder.create("orphans", UserGroup.get_admin_group())
    orphans: list[Block] = (
        run_sql(
            select(Block).filter(
                (Block.type_id == 0)
                & Block.id.notin_(select(DocEntry.id))
                & Block.id.notin_(select(Translation.doc_id))
            )
        )
        .scalars()
        .all()
    )

    for o in orphans:
        print(f"Adding a DocEntry for document with id {o.id}")
        d = DocEntry(id=o.id, name=f"{f.path}/orphan_{o.id}", public=True)
        db.session.add(d)
    print(
        f"Fixed {len(orphans)} documents without a DocEntry. They are in '{orphan_folder_title}' folder."
    )


def move_docs_without_block(dry_run: bool) -> None:
    """Moves all documents from tim_files/docs to tim_files/orphans that don't have a Block entry in database."""
    files_root = get_files_path()
    docs_folder = os.path.join(files_root, "docs")
    pars_folder = os.path.join(files_root, "pars")
    doc_folders = [f for f in os.listdir(docs_folder) if not isfile(f)]
    existing_blocks = {
        str(i)
        for i in run_sql(select(Block.id).filter_by(type_id=BlockType.Document.value))
        .scalars()
        .all()
    }
    docs_orphans = os.path.join(files_root, "orphans", "docs")
    pars_orphans = os.path.join(files_root, "orphans", "pars")
    if not dry_run:
        os.makedirs(docs_orphans, exist_ok=True)
        os.makedirs(pars_orphans, exist_ok=True)
    fixed = 0
    for f in doc_folders:
        if f not in existing_blocks:
            fixed += 1
            doc_dir = os.path.join(docs_folder, f)
            print(f"Moving {doc_dir} to orphans")
            if not dry_run:
                shutil.move(doc_dir, docs_orphans)
            par_dir = os.path.join(pars_folder, f)
            if os.path.exists(par_dir):
                print(f"Moving {par_dir} to orphans")
                if not dry_run:
                    shutil.move(par_dir, pars_orphans)
    print(f"Found {fixed} documents without a Block. They are in tim_files/orphans.")
