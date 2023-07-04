import shutil
from secrets import token_urlsafe

import click
from flask.cli import AppGroup
from sqlalchemy import select, delete

from timApp.admin.fix_orphan_documents import (
    fix_orphans_without_docentry,
    move_docs_without_block,
)
from timApp.admin.util import commit_if_not_dry
from timApp.document.docentry import DocEntry
from timApp.document.translation.translation import Translation
from timApp.item.block import Block, BlockType
from timApp.notification.notification import Notification
from timApp.notification.pending_notification import PendingNotification
from timApp.readmark.readparagraph import ReadParagraph
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.velp.velp_models import VelpGroupsInDocument

item_cli = AppGroup("item")


@item_cli.command("cleanup_default_rights_names")
def cleanup_default_right_doc_names() -> None:
    bs: list[Block] = db.session.scalars(
        select(Block).filter(
            Block.description.in_(
                [
                    "templates/DefaultDocumentRights",
                    "templates/DefaultFolderRights",
                    "$DefaultFolderRights",
                    "$DefaultDocumentRights",
                ]
            )
            & (Block.type_id == BlockType.Document.value)
        )
    ).all()
    num_changed = len(bs)
    for b in bs:
        b.description = b.description.replace("templates/", "").replace("$", "")
    db.session.commit()
    print(f"Fixed titles of {num_changed} default rights documents.")


@item_cli.command()
@click.option("--dry-run/--no-dry-run", default=True)
@click.option("--prompt-before-commit/--no-prompt-before-commit", default=False)
@click.option("--max-docs", default=None, type=int)
def cleanup_bookmark_docs(
    dry_run: bool, prompt_before_commit: bool, max_docs: int | None
) -> None:
    new_bookmark_users: list[User] = db.session.scalars(
        select(User).filter(User.prefs.contains('"bookmarks":'))
    ).all()
    docs_to_delete = set()
    processed_users = 0

    click.echo("Collecting bookmark documents")
    with click.progressbar(new_bookmark_users) as bar:
        for u in bar:  # type: User
            prefs = u.get_prefs()
            if not prefs.bookmarks:
                continue
            f = u.get_personal_folder()
            bm_doc = f.get_document("Bookmarks")
            if not bm_doc:
                continue
            docs_to_delete.add((u.name, bm_doc))
            processed_users += 1
            if max_docs and processed_users >= max_docs:
                click.echo(
                    f"Stopped at {processed_users} users because of max-docs cap"
                )
                break

    click.echo(f"Found {len(docs_to_delete)} bookmark documents to delete.")
    with click.progressbar(docs_to_delete) as bar:
        for u, bm_doc in bar:  # type: str, DocEntry
            click.echo(f"Deleting unused bookmarks document of {u}")
            block = bm_doc.block
            block.accesses = {}
            for t in (
                Translation,
                ReadParagraph,
                PendingNotification,
                VelpGroupsInDocument,
                Notification,
            ):
                db.session.execute(delete(t).where(t.doc_id == bm_doc.id))
            db.session.delete(bm_doc)
            db.session.delete(block)
    if dry_run:
        click.echo("Dry run enabled; nothing changed.")
        return

    if prompt_before_commit:
        if not click.confirm(
            f"{len(docs_to_delete)} users will be affected. Commit changes?"
        ):
            return
    db.session.commit()

    fp = get_files_path()
    deleted_folder = fp / "deleted"
    deleted_docs = deleted_folder / "docs"
    deleted_pars = deleted_folder / "pars"
    deleted_pars.mkdir(parents=True, exist_ok=True)
    deleted_docs.mkdir(parents=True, exist_ok=True)

    click.echo(f"Done, moving document files to {deleted_folder.as_posix()}...")
    failed_to_remove = []
    with click.progressbar(docs_to_delete) as bar:
        for u, bm_doc in bar:  # type: str, DocEntry
            d = bm_doc.id
            click.echo(f"Deleting bookmark document {d} (owned by {u})")
            doc_dir = fp / "docs" / str(d)
            pars_dir = fp / "pars" / str(d)
            try:
                shutil.move(doc_dir.as_posix(), deleted_docs)
                shutil.move(pars_dir.as_posix(), deleted_pars)
            except Exception as e:
                failed_to_remove.append(d)
                click.echo(f"Failed to remove {d}: {e}")
    click.echo(f"Removed {len(docs_to_delete) - len(failed_to_remove)} documents.")
    if failed_to_remove:
        click.echo("Failed to remove the following documents:")
        click.echo(failed_to_remove)


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
def verify_io() -> None:
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
