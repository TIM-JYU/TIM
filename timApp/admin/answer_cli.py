import json
from datetime import datetime
from typing import List, Tuple

import click
from flask.cli import AppGroup

from timApp.admin.datetimetype import DateTimeType
from timApp.answer.answer import Answer, AnswerSaver
from timApp.answer.answer_models import UserAnswer
from timApp.document.docentry import DocEntry
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup

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
    if not dry_run:
        db.session.commit()


@answer_cli.command()
@click.argument('doc')
@click.option('--dry-run/--no-dry-run', default=True)
def clear_all(doc: str, dry_run: bool) -> None:
    d = DocEntry.find_by_path(doc)
    if not d:
        click.echo(f'cannot find document "{doc}"', err=True)
        return
    ids = Answer.query.filter(Answer.task_id.startswith(f'{d.id}.')).with_entities(Answer.id)
    cnt = ids.count()
    UserAnswer.query.filter(UserAnswer.answer_id.in_(ids)).delete(synchronize_session=False)
    AnswerSaver.query.filter(AnswerSaver.answer_id.in_(ids)).delete(synchronize_session=False)
    Answer.query.filter(Answer.id.in_(ids)).delete(synchronize_session=False)
    click.echo(f'Total {cnt}')
    if not dry_run:
        db.session.commit()


@answer_cli.command()
@click.argument('doc')
@click.option('--deadline', type=DateTimeType(), required=True)
@click.option('--group', required=True)
@click.option('--dry-run/--no-dry-run', default=True)
def revalidate(doc: str, deadline: datetime, group: str, dry_run: bool) -> None:
    d = DocEntry.find_by_path(doc)
    if not d:
        click.echo(f'cannot find document "{doc}"', err=True)
        return
    answers: List[Tuple[Answer, str]] = (
        Answer.query
            .filter(Answer.task_id.startswith(f'{d.id}.'))
            .join(User, Answer.users)
            .join(UserGroup, User.groups)
            .filter(UserGroup.name == group)
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
        elif a.answered_on >= deadline and a.valid:
            changed_to_invalid += 1
            a.valid = False
            click.echo(f'Changing to invalid: {name}, {a.task_name}, {a.answered_on}, {a.points}')
    total = len(answers)
    click.echo(f'Changing {changed_to_valid} to valid, {changed_to_invalid} to invalid.')
    click.echo(f'Total answers in document for group: {total}')
    if not dry_run:
        db.session.commit()
    else:
        print('Dry run enabled, nothing changed.')
