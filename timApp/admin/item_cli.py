import shutil
from secrets import token_urlsafe

import click
from flask.cli import AppGroup

from timApp.admin.fix_orphan_documents import (
    fix_orphans_without_docentry,
    move_docs_without_block,
)
from timApp.admin.util import commit_if_not_dry
from timApp.document.docentry import DocEntry
from timApp.document.translation.translation import Translation
from timApp.item.block import Block, BlockType
from timApp.notification.pending_notification import PendingNotification
from timApp.readmark.readparagraph import ReadParagraph
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.velp.velp_models import VelpGroupsInDocument

item_cli = AppGroup("item")


@item_cli.command("cleanup_default_rights_names")
def cleanup_default_right_doc_names() -> None:
    bs: list[Block] = Block.query.filter(
        Block.description.in_(
            [
                "templates/DefaultDocumentRights",
                "templates/DefaultFolderRights",
                "$DefaultFolderRights",
                "$DefaultDocumentRights",
            ]
        )
        & (Block.type_id == BlockType.Document.value)
    ).all()
    num_changed = len(bs)
    for b in bs:
        b.description = b.description.replace("templates/", "").replace("$", "")
    db.session.commit()
    print(f"Fixed titles of {num_changed} default rights documents.")


@item_cli.command()
@click.option("--dry-run/--no-dry-run", default=True)
def cleanup_bookmark_docs(dry_run: bool) -> None:
    new_bookmark_users: list[User] = User.query.filter(
        User.prefs.contains('"bookmarks":')
    ).all()
    docs_to_delete = set()
    for u in new_bookmark_users:
        prefs = u.get_prefs()
        if not prefs.bookmarks:
            continue
        f = u.get_personal_folder()
        bm_doc = f.get_document("Bookmarks")
        if not bm_doc:
            continue
        print(f"Deleting unused bookmarks document of {u.name}")
        docs_to_delete.add(bm_doc.id)
        block = bm_doc.block
        block.accesses = {}
        Translation.query.filter_by(doc_id=bm_doc.id).delete()
        ReadParagraph.query.filter_by(doc_id=bm_doc.id).delete()
        PendingNotification.query.filter_by(doc_id=bm_doc.id).delete()
        VelpGroupsInDocument.query.filter_by(doc_id=bm_doc.id).delete()
        db.session.delete(bm_doc)
        db.session.delete(block)
    if dry_run:
        print("Dry run enabled; nothing changed.")
        return
    db.session.commit()
    fp = get_files_path()
    deleted_docs = fp / "deleted" / "docs"
    deleted_pars = fp / "deleted" / "pars"
    deleted_pars.mkdir(parents=True, exist_ok=True)
    deleted_docs.mkdir(parents=True, exist_ok=True)
    for d in docs_to_delete:
        doc_dir = fp / "docs" / str(d)
        pars_dir = fp / "pars" / str(d)
        shutil.move(doc_dir.as_posix(), deleted_docs)
        shutil.move(pars_dir.as_posix(), deleted_pars)


@item_cli.command()
@click.option("--dry-run/--no-dry-run", default=True)
def fix_orphans(dry_run: bool) -> None:
    """Finds and fixes or cleans up orphaned documents.

    * Finds all documents (in Block table) that do not have a DocEntry and
      creates a DocEntry for them under 'orphans' directory.
    * Moves all documents from tim_files/docs to tim_files/orphans that don't have a Block entry in database.
    """
    fix_orphans_without_docentry()
    move_docs_without_block(dry_run)
    commit_if_not_dry(dry_run)


@item_cli.command()
def verify_io():
    """Basic IO test to verify that documents can be created on the current TIM install"""

    click.echo("Testing basic IO")
    folder_name = token_urlsafe(10)
    click.echo("Creating a document")
    doc_path = f"users/{folder_name}/tmp"
    DocEntry.create(doc_path, title="Test document")
    db.session.commit()

    d = DocEntry.find_by_path(doc_path)
    if d:
        click.echo("Document seems to exist")
    else:
        click.echo("No document found! An IO error or DB error?")
        exit(1)

    click.echo("Adding some paragraphs")
    d.document.add_text(
        """
# Test

This is a test

#-
Second paragraph
"""
    )

    db.session.commit()

    d = DocEntry.find_by_path(doc_path)
    db.session.refresh(d)
    click.echo("Reading added paragraphs back")

    d.document.par_cache = None
    d.document.load_pars()
    res = d.document.export_markdown(export_ids=False)
    expected = """# Test

This is a test

Second paragraph
"""
    if res == expected:
        click.echo("Loaded markdown appears correct")
    else:
        click.echo("Loaded unexpected markdown!")
        click.echo(res)
        exit(1)

    click.echo("Deleting document")
    block = d.block
    block.accesses = {}
    db.session.delete(d)
    db.session.delete(block)
    db.session.commit()

    fp = get_files_path()
    deleted_docs = fp / "deleted" / "docs"
    deleted_pars = fp / "deleted" / "pars"
    deleted_pars.mkdir(parents=True, exist_ok=True)
    deleted_docs.mkdir(parents=True, exist_ok=True)
    doc_dir = fp / "docs" / str(d.id)
    pars_dir = fp / "pars" / str(d.id)
    shutil.move(doc_dir.as_posix(), deleted_docs)
    shutil.move(pars_dir.as_posix(), deleted_pars)

    click.echo("Done, basic IO seems to work!")
