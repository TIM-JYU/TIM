import json
import sys
from dataclasses import dataclass
from datetime import datetime
from typing import List, Tuple, Sequence

import click
from flask.cli import AppGroup
from sqlalchemy import func
from sqlalchemy.orm import joinedload

from timApp.admin.datetimetype import DateTimeType
from timApp.admin.timitemtype import TimDocumentType, TimItemType
from timApp.admin.util import commit_if_not_dry
from timApp.answer.answer import Answer, AnswerSaver
from timApp.answer.answer_models import UserAnswer, AnswerUpload
from timApp.answer.answers import valid_answers_query
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.item import Item
from timApp.plugin.taskid import TaskId
from timApp.upload.uploadedfile import PluginUpload
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.pdftools import is_pdf_producer_ghostscript, compress_pdf, CompressionError
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


@dataclass
class AnswerDeleteResult:
    useranswer: int
    answersaver: int
    annotation: int
    annotationcomment: int
    answer: int


@answer_cli.command()
@click.argument('doc', type=TimDocumentType())
@click.option('--dry-run/--no-dry-run', default=True)
def clear_all(doc: DocInfo, dry_run: bool) -> None:
    ids = Answer.query.filter(Answer.task_id.startswith(f'{doc.id}.')).with_entities(Answer.id).all()
    cnt = len(ids)
    delete_answers_with_ids(ids)
    click.echo(f'Total {cnt}')
    commit_if_not_dry(dry_run)


@answer_cli.command()
@click.argument('doc', type=TimDocumentType())
@click.option('--dry-run/--no-dry-run', default=True)
@click.option('--task', '-t', multiple=True)
def clear(doc: DocInfo, dry_run: bool, task: List[str]) -> None:
    tasks_to_delete = [f'{doc.id}.{t}' for t in task]
    ids = Answer.query.filter(Answer.task_id.in_(tasks_to_delete)).with_entities(Answer.id).all()
    cnt = len(ids)
    delete_answers_with_ids(ids)
    click.echo(f'Total {cnt}')
    commit_if_not_dry(dry_run)


def delete_answers_with_ids(ids: List[int]) -> AnswerDeleteResult:
    if not isinstance(ids, list):
        raise TypeError('ids should be a list of answer ids')
    d_ua = UserAnswer.query.filter(UserAnswer.answer_id.in_(ids)).delete(synchronize_session=False)
    d_as = AnswerSaver.query.filter(AnswerSaver.answer_id.in_(ids)).delete(synchronize_session=False)
    anns = Annotation.query.filter(Annotation.answer_id.in_(ids))
    d_acs = AnnotationComment.query.filter(AnnotationComment.annotation_id.in_(anns.with_entities(Annotation.id))).delete(
        synchronize_session=False)
    d_anns = anns.delete(synchronize_session=False)
    d_ans = Answer.query.filter(Answer.id.in_(ids)).delete(synchronize_session=False)
    return AnswerDeleteResult(
        useranswer=d_ua,
        answersaver=d_as,
        annotation=d_anns,
        annotationcomment=d_acs,
        answer=d_ans,
    )


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
    docs = collect_docs(item)
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
                    try:
                        old_size = uf.size
                    except FileNotFoundError:
                        click.echo(f'PDF {uf.relative_filesystem_path} not found.')
                        continue
                    if old_size == 0:
                        click.echo(f'PDF {uf.relative_filesystem_path} has size 0; skipping.')
                        continue
                    if not uf.is_content_pdf:
                        click.echo(f'PDF {uf.relative_filesystem_path} content is not PDF; skipping.')
                        continue
                    click.echo(f'Compressing PDF {uf.relative_filesystem_path}... ', nl=False)
                    try:
                        compress_pdf(uf)
                    except CompressionError:
                        click.echo(f'Failed to compress PDF {uf.relative_filesystem_path}; it may be corrupted.')
                        continue
                    new_size = uf.size
                    percent = round((old_size - new_size) / old_size * 100)
                    click.echo(f'done, size: {old_size} -> {new_size} (reduced by {percent}%)')


def collect_docs(item: Item) -> Sequence[DocInfo]:
    if isinstance(item, Folder):
        docs: Sequence[DocInfo] = item.get_all_documents(include_subdirs=True)
    elif isinstance(item, DocInfo):
        docs = [item]
    else:
        raise Exception('Unknown item type')
    return docs


@dataclass
class DeleteResult:
    total: int
    deleted: int
    adr: AnswerDeleteResult

    @property
    def remaining(self) -> int:
        return self.total - self.deleted


def delete_old_answers(d: DocInfo, tasks: List[str]) -> DeleteResult:
    base_query = (
        valid_answers_query([TaskId(doc_id=d.id, task_name=t) for t in tasks])
            .join(User, Answer.users)
    )
    latest = (
        base_query
            .group_by(Answer.task_id, User.id)
            .with_entities(func.max(Answer.id))
    )
    todelete = base_query.filter(Answer.id.notin_(latest)).with_entities(Answer.id)
    tot = base_query.count()
    del_tot = todelete.count()
    adr = delete_answers_with_ids(todelete.all())
    r = DeleteResult(total=tot, deleted=del_tot, adr=adr)
    return r


@answer_cli.command()
@click.argument('item', type=TimItemType())
@click.option('--task', '-t', multiple=True)
@click.option('--dry-run/--no-dry-run', default=True)
def delete_old(item: Item, task: List[str], dry_run: bool) -> None:
    """Deletes all older than latest answers from the specified tasks.

    This is useful especially for deleting field history in documents where jsrunner is used a lot.
    """
    docs = collect_docs(item)
    for d in docs:
        r = delete_old_answers(d, task)
        click.echo(f'Deleting {r.deleted} of {r.total} answers from {d.path}, remaining {r.remaining}. {r.adr}')
    commit_if_not_dry(dry_run)
