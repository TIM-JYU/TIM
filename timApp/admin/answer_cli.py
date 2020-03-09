import json
from datetime import datetime
from typing import List

import click
from flask.cli import AppGroup

from timApp.answer.answer import Answer, AnswerSaver
from timApp.answer.answer_models import UserAnswer
from timApp.document.docentry import DocEntry
from timApp.timdb.sqa import db

answer_cli = AppGroup('answer')


@answer_cli.command()
@click.option('--dry-run/--no-dry-run', default=True)
def fix_double_c(dry_run):
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
def clear_all(doc, dry_run):
    d = DocEntry.find_by_path(doc)
    if not d:
        click.echo(err=f'cannot find document "{doc}"')
        return
    ids = Answer.query.filter(Answer.task_id.startswith(f'{d.id}.')).with_entities(Answer.id)
    cnt = ids.count()
    UserAnswer.query.filter(UserAnswer.answer_id.in_(ids)).delete(synchronize_session=False)
    AnswerSaver.query.filter(AnswerSaver.answer_id.in_(ids)).delete(synchronize_session=False)
    Answer.query.filter(Answer.id.in_(ids)).delete(synchronize_session=False)
    click.echo(f'Total {cnt}')
    if not dry_run:
        db.session.commit()
