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
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from timApp.velp.velp_models import VelpGroupsInDocument

item_cli = AppGroup("item")


@item_cli.command("cleanup_default_rights_names")
def cleanup_default_right_doc_names() -> None:
    bs: list[Block] = (
        run_sql(
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
        )
        .scalars()
        .all()
    )
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
    new_bookmark_users: list[User] = (
        run_sql(select(User).filter(User.prefs.contains('"bookmarks":')))
        .scalars()
        .all()
    )
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
                run_sql(delete(t).where(t.doc_id == bm_doc.id))
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


@item_cli.command("delete")
@click.option("--dry-run/--no-dry-run", default=True)
@click.option(
    "-i",
    "--id",
    "item_id",
    type=int,
    required=True,
    prompt="ID of the item to delete",
)
def permanent_delete(item_id: int, dry_run: bool) -> None:
    """
    Permanently deletes the item from the database and its associated files on disk.
    :param item_id: ID of the item to delete
    :param dry_run: Whether to perform a dry run or not.
    :return:
    """

    item = db.session.get(Block, item_id)
    if not item:
        click.echo(f"Item with ID {item_id} was not found.")
    if item.type_id not in [BlockType.Document.value, BlockType.Folder.value]:
        click.echo(
            f"Permanent deletion is currently only supported for Document and Folder items."
        )
        return

    # TODO Add keyboard confirmation prompt (ie. 'Type [Yes/No] to confirm')

    # Prepare Block for deletion
    item.description = f"deleted_{item_id}"
    item.accesses = dict()

    from timApp.user.usergroup import UserGroup

    admin_ug = UserGroup.get_admin_group()
    item.set_owner(admin_ug)

    deleted = []
    match item.type_id:
        case BlockType.Document.value:
            deleted = perma_del_doc(item_id)
        case BlockType.Folder.value:
            deleted = perma_del_folder(item_id)

    if not dry_run:
        db.session.commit()

        click.echo(f"Successfully deleted files and entries for item ID {item_id}")
        click.echo(f"Deleted files and entries:")
        for entry in deleted:
            click.echo(f" - {entry}")
    else:
        click.echo(f"*** DRY RUN Results ***")
        click.echo(f"Performing the action with dry_run=False")
        click.echo("will *permanently* delete the following files and entries:")
        for entry in deleted:
            click.echo(f" - {entry}")


def perma_del_doc(item_id: int, dry_run: bool) -> list[str]:
    deleted: list[str] = []

    # Clear DocEntry db objects linked to this ID
    des = DocEntry.find_all_by_id(item_id)
    tr_ids = set()
    for de in des:
        # Get IDs for this document's Translations, if any, so that they can be deleted as well
        for tr in de.translations:
            tr_ids.add(tr.id)
        if not dry_run:
            db.session.delete(de)
        deleted.append(de.name)
    if not dry_run:
        deleted_placeholder = DocEntry()
        deleted_placeholder.name = f"roskis/$deleted_{item_id}"
        deleted_placeholder.id = item_id
        deleted_placeholder.public = False

        db.session.add(deleted_placeholder)

    # Clear disk files and folders related to this document
    from pathlib import Path

    pars_path = Path(f"{get_files_path()}/pars/{item_id}")
    ver_path = Path(f"{get_files_path()}/docs/{item_id}")

    if not dry_run:
        if pars_path:
            pars_path.unlink()
        if ver_path:
            ver_path.unlink()

    # Create an empty Document, which reserves the original ID.
    # As a safety measure, we want to prevent re-assigning a previously used ID.
    from timApp.document.document import Document

    if not dry_run:
        d_reserved = Document(doc_id=item_id).create()
        db.session.add(d_reserved)

    for i in tr_ids:
        perma_del_doc(i, dry_run=dry_run)

    return deleted


def perma_del_folder(item_id: int, dry_run: bool) -> list[str]:
    deleted: list[str] = []

    # Gather folder contents
    from timApp.folder.folder import Folder
    from timApp.document.docentry import get_documents

    folder = Folder.find_by_id(item_id)
    # docs: list[Document] = folder.get_all_documents() # we don't want to prematurely recurse sub-dirs
    docs: list[DocEntry] = get_documents(
        include_nonpublic=True, filter_folder=folder.path, search_recursively=False
    )
    subfolders: list[Folder] = folder.get_all_folders()

    deleted.append(f"{folder.path}")

    folder.name = f"$deleted_{item_id}"
    folder.location = f"roskis"

    for doc in docs:
        deleted_docs = perma_del_doc(doc.id, dry_run=dry_run)
        deleted.extend(deleted_docs)

    for subfolder in subfolders:
        deleted_subfolders = perma_del_folder(subfolder.id)
        deleted.extend(deleted_subfolders)

    return deleted
