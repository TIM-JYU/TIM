import json
import sys
from dataclasses import dataclass
from datetime import datetime
from typing import Sequence

import click
from flask.cli import AppGroup
from sqlalchemy import func, select, delete
from sqlalchemy.orm import selectinload

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
from timApp.timdb.sqa import db, run_sql
from timApp.upload.uploadedfile import PluginUpload
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.pdftools import (
    is_pdf_producer_ghostscript,
    compress_pdf,
    CompressionError,
)
from timApp.velp.annotation_model import Annotation
from timApp.velp.velp_models import AnnotationComment

answer_cli = AppGroup("answer")


@answer_cli.command()
@click.option("--dry-run/--no-dry-run", default=True)
def fix_double_c(dry_run: bool) -> None:
    answers = (
        run_sql(
            select(Answer)
            .filter(
                (Answer.answered_on > datetime(year=2020, month=2, day=9))
                & Answer.content.startswith('{"c": {"c":')
            )
            .order_by(Answer.id)
        )
        .scalars()
        .all()
    )
    count = 0
    for a in answers:
        cont = a.content_as_json
        if not isinstance(cont, dict):
            continue
        c = cont.get("c")
        if isinstance(c, dict):
            if "c" in c:
                print(f"Modifying {a.id} ({a.task_id}, {a.answered_on})")
                count += 1
                if not dry_run:
                    a.content = json.dumps(c)
    print(f"Total {count}")
    commit_if_not_dry(dry_run)


@dataclass
class AnswerDeleteResult:
    useranswer: int
    answersaver: int
    annotation: int
    annotationcomment: int
    answer: int


@answer_cli.command()
@click.argument("doc", type=TimDocumentType())
@click.option("--dry-run/--no-dry-run", default=True)
def clear_all(doc: DocInfo, dry_run: bool) -> None:
    ids = (
        run_sql(select(Answer.id).filter(Answer.task_id.startswith(f"{doc.id}.")))
        .scalars()
        .all()
    )

    cnt = len(ids)
    delete_answers_with_ids(ids)
    click.echo(f"Total {cnt}")
    commit_if_not_dry(dry_run)


@answer_cli.command()
@click.argument("doc", type=TimDocumentType())
@click.option("--dry-run/--no-dry-run", default=True)
@click.option("--task", "-t", multiple=True)
@click.option("--answer_from", "-af", type=click.DateTime(), default=None)
@click.option("--answer_to", "-at", type=click.DateTime(), default=None)
@click.option("--verbose/--no-verbose", "-v", default=False)
def clear(
    doc: DocInfo,
    dry_run: bool,
    task: list[str],
    answer_from: datetime | None,
    answer_to: datetime | None,
    verbose: bool,
) -> None:
    tasks_to_delete = [f"{doc.id}.{t}" for t in task]
    stmt = select(Answer.id).filter(Answer.task_id.in_(tasks_to_delete))
    if answer_from:
        stmt = stmt.filter(Answer.answered_on >= answer_from)
    if answer_to:
        stmt = stmt.filter(Answer.answered_on <= answer_to)
    ids = run_sql(stmt).scalars().all()
    cnt = len(ids)
    result = delete_answers_with_ids(ids, verbose)
    click.echo(f"Total {cnt}")
    commit_if_not_dry(dry_run)


def delete_answers_with_ids(
    ids: list[int], verbose: bool = False
) -> AnswerDeleteResult:
    if not isinstance(ids, list):
        raise TypeError("ids should be a list of answer ids")
    d_ua = len(
        run_sql(
            delete(UserAnswer)
            .where(UserAnswer.answer_id.in_(ids))
            .returning(UserAnswer.id)
            .execution_options(synchronize_session=False)
        ).all()
    )
    d_as = len(
        (
            run_sql(
                delete(AnswerSaver)
                .where(AnswerSaver.answer_id.in_(ids))
                .returning(AnswerSaver.user_id, AnswerSaver.answer_id)
                .execution_options(synchronize_session=False)
            ).all()
        )
    )
    anns_stmt = select(Annotation.id).filter(Annotation.answer_id.in_(ids))
    d_acs = len(
        (
            run_sql(
                delete(AnnotationComment)
                .where(
                    AnnotationComment.annotation_id.in_(
                        anns_stmt.with_only_columns(Annotation.id)
                    )
                )
                .returning(AnnotationComment.id)
                .execution_options(synchronize_session=False)
            ).all()
        )
    )
    d_anns = len(
        (
            run_sql(
                delete(Annotation)
                .where(Annotation.id.in_(anns_stmt))
                .returning(Annotation.id)
                .execution_options(synchronize_session=False)
            ).all()
        )
    )
    ans_items_stmt = select(Answer).filter(Answer.id.in_(ids))
    if verbose:
        click.echo(
            "\n".join(
                [
                    f"taskid: {a.task_id}, points: {a.points}, answered_on: {a.answered_on}; saver: {a.saver}"
                    for a in run_sql(ans_items_stmt).scalars()
                ]
            )
        )
    d_ans = len(
        run_sql(
            delete(Answer)
            .where(Answer.id.in_(ans_items_stmt.with_only_columns(Answer.id)))
            .returning(Answer.id)
            .execution_options(synchronize_session=False)
        ).all()
    )
    return AnswerDeleteResult(
        useranswer=d_ua,
        answersaver=d_as,
        annotation=d_anns,
        annotationcomment=d_acs,
        answer=d_ans,
    )


@answer_cli.command()
@click.argument("doc", type=TimDocumentType())
@click.option("--deadline", type=DateTimeType(), required=True)
@click.option("--group", required=True)
@click.option("--dry-run/--no-dry-run", default=True)
@click.option("--may-invalidate/--no-may-invalidate", default=False)
def revalidate(
    doc: DocInfo, deadline: datetime, group: str, dry_run: bool, may_invalidate: bool
) -> None:
    answers: list[tuple[Answer, str]] = (
        run_sql(
            select(Answer, User.name)
            .join(User, Answer.users)
            .join(UserGroup, User.groups)
            .filter(Answer.task_id.startswith(f"{doc.id}."))
            .order_by(Answer.answered_on.desc())
        )
        .scalars()
        .all()
    )

    changed_to_valid = 0
    changed_to_invalid = 0
    for a, name in answers:
        if a.answered_on < deadline and not a.valid:
            changed_to_valid += 1
            a.valid = True
            click.echo(
                f"Changing to valid: {name}, {a.task_name}, {a.answered_on}, {a.points}"
            )
        elif a.answered_on >= deadline and a.valid and may_invalidate:
            changed_to_invalid += 1
            a.valid = False
            click.echo(
                f"Changing to invalid: {name}, {a.task_name}, {a.answered_on}, {a.points}"
            )
    total = len(answers)
    click.echo(
        f"Changing {changed_to_valid} to valid, {changed_to_invalid} to invalid."
    )
    click.echo(f"Total answers in document for group: {total}")
    commit_if_not_dry(dry_run)


@answer_cli.command()
@click.argument("doc", type=TimDocumentType())
@click.option("--limit", required=True, type=int)
@click.option("--to", required=True, type=int)
@click.option("--dry-run/--no-dry-run", default=True)
def truncate_large(doc: DocInfo, limit: int, to: int, dry_run: bool) -> None:
    if limit < to:
        click.echo("limit must be >= to")
        sys.exit(1)
    stmt = select(Answer).filter(Answer.task_id.startswith(f"{doc.id}."))
    total = db.session.scalar(stmt.with_only_columns(func.count()))
    anss: list[Answer] = (
        run_sql(
            stmt.filter(func.length(Answer.content) > limit).options(
                selectinload(Answer.users_all)
            )
        )
        .scalars()
        .all()
    )
    note = " (answer truncated)"
    try_keys = ["usercode", "c", "userinput"]
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
                        new_c = c[: -(c_diff + len(note))] + note
                    except IndexError:
                        continue
                    name = a.users_all[0].name if a.users_all else "(orphan)"
                    print(
                        f"Truncating: {a.task_id}, {name}, {a.answered_on}, length {len(a.content)}"
                    )
                    truncated += 1
                    loaded[k] = new_c
                    a.content = json.dumps(loaded)
                    break
    print(f"Truncating {truncated} answers (out of {total}).")
    commit_if_not_dry(dry_run)


@answer_cli.command()
@click.argument("item", type=TimItemType())
@click.option("--dry-run/--no-dry-run", default=True)
def compress_uploads(item: Item, dry_run: bool) -> None:
    docs = collect_docs(item)
    for d in docs:
        uploads: list[Block] = (
            run_sql(
                select(Block)
                .select_from(Answer)
                .filter(Answer.task_id.startswith(f"{d.id}."))
                .join(AnswerUpload)
                .join(Block)
            )
            .scalars()
            .all()
        )
        for u in uploads:
            path = u.description
            if path.lower().endswith(".pdf"):
                uf = PluginUpload(u)
                if is_pdf_producer_ghostscript(uf):
                    click.echo(
                        f"Skipping already processed PDF {uf.relative_filesystem_path}"
                    )
                else:
                    if dry_run:
                        click.echo(f"Would compress PDF {uf.relative_filesystem_path}")
                        continue
                    try:
                        old_size = uf.size
                    except FileNotFoundError:
                        click.echo(f"PDF {uf.relative_filesystem_path} not found.")
                        continue
                    if old_size == 0:
                        click.echo(
                            f"PDF {uf.relative_filesystem_path} has size 0; skipping."
                        )
                        continue
                    if not uf.is_content_pdf:
                        click.echo(
                            f"PDF {uf.relative_filesystem_path} content is not PDF; skipping."
                        )
                        continue
                    click.echo(
                        f"Compressing PDF {uf.relative_filesystem_path}... ", nl=False
                    )
                    try:
                        compress_pdf(uf)
                    except CompressionError:
                        click.echo(
                            f"Failed to compress PDF {uf.relative_filesystem_path}; it may be corrupted."
                        )
                        continue
                    new_size = uf.size
                    percent = round((old_size - new_size) / old_size * 100)
                    click.echo(
                        f"done, size: {old_size} -> {new_size} (reduced by {percent}%)"
                    )


def collect_docs(item: Item) -> Sequence[DocInfo]:
    if isinstance(item, Folder):
        docs: Sequence[DocInfo] = item.get_all_documents(include_subdirs=True)
    elif isinstance(item, DocInfo):
        docs = [item]
    else:
        raise Exception("Unknown item type")
    return docs


@dataclass
class DeleteResult:
    total: int
    deleted: int
    adr: AnswerDeleteResult

    @property
    def remaining(self) -> int:
        return self.total - self.deleted


def delete_old_answers(d: DocInfo, tasks: list[str]) -> DeleteResult:
    base_query = valid_answers_query(
        [TaskId(doc_id=d.id, task_name=t) for t in tasks]
    ).join(User, Answer.users)
    latest = base_query.group_by(Answer.task_id, User.id).with_only_columns(
        func.max(Answer.id)
    )
    todelete = base_query.filter(Answer.id.notin_(latest)).with_only_columns(Answer.id)
    tot = db.session.scalar(base_query.with_only_columns(func.count()))
    del_tot = db.session.scalar(todelete.with_only_columns(func.count()))
    adr = delete_answers_with_ids(run_sql(todelete).scalars().all())
    r = DeleteResult(total=tot, deleted=del_tot, adr=adr)
    return r


@answer_cli.command()
@click.argument("item", type=TimItemType())
@click.option("--task", "-t", multiple=True)
@click.option("--dry-run/--no-dry-run", default=True)
def delete_old(item: Item, task: list[str], dry_run: bool) -> None:
    """Deletes all older than latest answers from the specified tasks.

    This is useful especially for deleting field history in documents where jsrunner is used a lot.
    """
    docs = collect_docs(item)
    for d in docs:
        r = delete_old_answers(d, task)
        click.echo(
            f"Deleting {r.deleted} of {r.total} answers from {d.path}, remaining {r.remaining}. {r.adr}"
        )
    commit_if_not_dry(dry_run)
