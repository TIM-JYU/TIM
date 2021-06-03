from typing import List

import shutil

import click
from flask.cli import AppGroup

from timApp.document.translation.translation import Translation
from timApp.item.block import Block, BlockType
from timApp.notification.pending_notification import PendingNotification
from timApp.readmark.readparagraph import ReadParagraph
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.velp.velp_models import VelpGroupsInDocument

item_cli = AppGroup('item')


@item_cli.command('cleanup_default_rights_names')
def cleanup_default_right_doc_names() -> None:
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


@item_cli.command()
@click.option('--dry-run/--no-dry-run', default=True)
def cleanup_bookmark_docs(dry_run: bool) -> None:
    new_bookmark_users: List[User] = User.query.filter(User.prefs.contains('"bookmarks":')).all()
    docs_to_delete = set()
    for u in new_bookmark_users:
        prefs = u.get_prefs()
        if not prefs.bookmarks:
            continue
        f = u.get_personal_folder()
        bm_doc = f.get_document('Bookmarks')
        if not bm_doc:
            continue
        print(f'Deleting unused bookmarks document of {u.name}')
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
        print('Dry run enabled; nothing changed.')
        return
    db.session.commit()
    fp = get_files_path()
    deleted_docs = fp / 'deleted' / 'docs'
    deleted_pars = fp / 'deleted' / 'pars'
    deleted_pars.mkdir(parents=True, exist_ok=True)
    deleted_docs.mkdir(parents=True, exist_ok=True)
    for d in docs_to_delete:
        doc_dir = fp / 'docs' / str(d)
        pars_dir = fp / 'pars' / str(d)
        shutil.move(doc_dir.as_posix(), deleted_docs)
        shutil.move(pars_dir.as_posix(), deleted_pars)
