from typing import List

import click
from flask.cli import AppGroup
from sqlalchemy import true

from timApp.sisu.parse_display_name import parse_sisu_group_display_name
from timApp.sisu.sisu import refresh_sisu_grouplist_doc, send_course_group_mail
from timApp.timdb.sqa import db
from timApp.user.usergroup import get_sisu_groups_by_filter, UserGroup

sisu_cli = AppGroup('sisu')


@sisu_cli.command('createdocs')
def create_docs():
    all_sisu_groups = get_sisu_groups_by_filter(true())
    for g in all_sisu_groups:
        print(f'Refreshing {g.external_id.external_id}')
        refresh_sisu_grouplist_doc(g)
    db.session.commit()


@sisu_cli.command('sendmail')
@click.argument('courses', nargs=-1)
def send_course_mail_cli(courses: List[str]):
    for course in courses:
        ug = UserGroup.get_by_external_id(f'{course}-responsible-teachers')
        if not ug:
            print(f'Could not find the responsible teachers group for course {course}. '
                  'Make sure you typed the course in format "jy-CUR-xxxx".')
            return
        p = parse_sisu_group_display_name(ug.display_name)
        for u in ug.users:
            print(f'Sending mail to {u.real_name} {u.email}')
            send_course_group_mail(p, u)
