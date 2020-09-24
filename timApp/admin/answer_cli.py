import json
import sys
from datetime import datetime
from typing import List, Tuple, Sequence

import click
from flask.cli import AppGroup
from sqlalchemy import func
from sqlalchemy.orm import joinedload

from timApp.admin.datetimetype import DateTimeType
from timApp.admin.timitemtype import TimDocumentType, TimItemType
from timApp.answer.answer import Answer, AnswerSaver
from timApp.answer.answer_models import UserAnswer, AnswerUpload
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.item import Item
from timApp.timdb.sqa import db
from timApp.upload.uploadedfile import PluginUpload
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.pdftools import is_pdf_producer_ghostscript, compress_pdf
from timApp.velp.annotation_model import Annotation
from timApp.velp.velp_models import AnnotationComment

answer_cli = AppGroup('answer')


@answer_cli.command()
@click.option('--dry-run/--no-dry-run', default=True)
def fix_double_c(dry_run: bool) -> None:
    answers: List[Answer] = (
        Answer.query
            .filter((Answer.answered_on > datetime(year=2020, month=2, day=9)) & Answer.content.startswith('{"c": {"c":'))
            .order_by(Answer.id)
            .all()
    )
    count = 0
    for a in answers:
        cont = a.content_as_json
        if not isinstance(cont, dict):
            continue
        c = cont.get('c')
        if isinstance(c, dict):
            if 'c' in c:
                print(f'Modifying {a.id} ({a.task_id}, {a.answered_on})')
                count += 1
                if not dry_run:
                    a.content = json.dumps(c)
    print(f'Total {count}')
    commit_if_not_dry(dry_run)


@answer_cli.command()
@click.argument('doc', type=TimDocumentType())
@click.option('--dry-run/--no-dry-run', default=True)
def clear_all(doc: DocInfo, dry_run: bool) -> None:
    ids = Answer.query.filter(Answer.task_id.startswith(f'{doc.id}.')).with_entities(Answer.id)
    cnt = ids.count()
    UserAnswer.query.filter(UserAnswer.answer_id.in_(ids)).delete(synchronize_session=False)
    AnswerSaver.query.filter(AnswerSaver.answer_id.in_(ids)).delete(synchronize_session=False)
    anns = Annotation.query.filter(Annotation.answer_id.in_(ids))
    AnnotationComment.query.filter(AnnotationComment.annotation_id.in_(anns.with_entities(Annotation.id))).delete(synchronize_session=False)
    anns.delete(synchronize_session=False)
    Answer.query.filter(Answer.id.in_(ids)).delete(synchronize_session=False)
    click.echo(f'Total {cnt}')
    commit_if_not_dry(dry_run)


@answer_cli.command()
@click.argument('doc', type=TimDocumentType())
@click.option('--deadline', type=DateTimeType(), required=True)
@click.option('--group', required=True)
@click.option('--dry-run/--no-dry-run', default=True)
@click.option('--may-invalidate/--no-may-invalidate', default=False)
def revalidate(doc: DocInfo, deadline: datetime, group: str, dry_run: bool, may_invalidate: bool) -> None:
    answers: List[Tuple[Answer, str]] = (
        Answer.query
            .filter(Answer.task_id.startswith(f'{doc.id}.'))
            .join(User, Answer.users)
            .join(UserGroup, User.groups)
            .filter(UserGroup.name == group)
            .order_by(Answer.answered_on.desc())
            .with_entities(Answer, User.name)
            .all()
    )
    changed_to_valid = 0
    changed_to_invalid = 0
    for a, name in answers:
        if a.answered_on < deadline and not a.valid:
            changed_to_valid += 1
            a.valid = True
            click.echo(f'Changing to valid: {name}, {a.task_name}, {a.answered_on}, {a.points}')
        elif a.answered_on >= deadline and a.valid and may_invalidate:
            changed_to_invalid += 1
            a.valid = False
            click.echo(f'Changing to invalid: {name}, {a.task_name}, {a.answered_on}, {a.points}')
    total = len(answers)
    click.echo(f'Changing {changed_to_valid} to valid, {changed_to_invalid} to invalid.')
    click.echo(f'Total answers in document for group: {total}')
    commit_if_not_dry(dry_run)


def commit_if_not_dry(dry_run: bool) -> None:
    if not dry_run:
        db.session.commit()
    else:
        click.echo('Dry run enabled, nothing changed.')


@answer_cli.command()
@click.argument('doc', type=TimDocumentType())
@click.option('--limit', required=True, type=int)
@click.option('--to', required=True, type=int)
@click.option('--dry-run/--no-dry-run', default=True)
def truncate_large(doc: DocInfo, limit: int, to: int, dry_run: bool) -> None:
    if limit < to:
        click.echo('limit must be >= to')
        sys.exit(1)
    q = Answer.query.filter(Answer.task_id.startswith(f'{doc.id}.'))
    total = q.count()
    anss: List[Answer] = (
        q
        .filter(func.length(Answer.content) > limit)
        .options(joinedload(Answer.users_all))
        .all()
    )
    note = ' (answer truncated)'
    try_keys = ['usercode', 'c', 'userinput']
    truncated = 0
    for a in anss:
        diff = len(a.content) - to
        if diff > 0:
            loaded = a.content_as_json
            if not isinstance(loaded, dict):
                continue
            for k in try_keys:
                c = loaded.get(k)
                if c:
                    c_diff = len(c) - to
                    if c_diff <= 0:
                        continue
                    try:
                        new_c = c[:-(c_diff + len(note))] + note
                    except IndexError:
                        continue
                    name = a.users_all[0].name if a.users_all else '(orphan)'
                    print(f'Truncating: {a.task_id}, {name}, {a.answered_on}, length {len(a.content)}')
                    truncated += 1
                    loaded[k] = new_c
                    a.content = json.dumps(loaded)
                    break
    print(f'Truncating {truncated} answers (out of {total}).')
    commit_if_not_dry(dry_run)


@answer_cli.command()
@click.argument('item', type=TimItemType())
@click.option('--dry-run/--no-dry-run', default=True)
def compress_uploads(item: Item, dry_run: bool) -> None:
    if isinstance(item, Folder):
        docs: Sequence[DocInfo] = item.get_all_documents(include_subdirs=True)
    elif isinstance(item, DocInfo):
        docs = [item]
    else:
        raise Exception('Unknown item type')
    for d in docs:
        uploads: List[Block] = (
            Answer.query
                .filter(Answer.task_id.startswith(f'{d.id}.'))
                .join(AnswerUpload)
                .join(Block)
                .with_entities(Block)
                .all()
        )
        for u in uploads:
            path = u.description
            if path.lower().endswith('.pdf'):
                uf = PluginUpload(u)
                if is_pdf_producer_ghostscript(uf):
                    click.echo(f'Skipping already processed PDF {uf.relative_filesystem_path}')
                else:
                    if dry_run:
                        click.echo(f'Would compress PDF {uf.relative_filesystem_path}')
                        continue
                    old_size = uf.size
                    click.echo(f'Compressing PDF {uf.relative_filesystem_path}... ', nl=False)
                    compress_pdf(uf)
                    new_size = uf.size
                    percent = round((old_size - new_size) / old_size * 100)
                    click.echo(f'done, size: {old_size} -> {new_size} (reduced by {percent}%)')
