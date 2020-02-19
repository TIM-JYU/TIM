import json
from datetime import datetime
from typing import List

import click
from flask.cli import AppGroup

from timApp.answer.answer import Answer
from timApp.timdb.sqa import db

answer_cli = AppGroup('answer')


@answer_cli.command('fix_double_c')
@click.option('--dry-run/--no-dry-run', default=True)
def create_docs(dry_run):
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
