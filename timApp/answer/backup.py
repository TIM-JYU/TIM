from pathlib import Path

import filelock
from flask import current_app, Response

from timApp.answer.answer import Answer
from timApp.answer.exportedanswer import ExportedAnswer
from timApp.document.docentry import DocEntry
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import to_json_str, ok_response
from timApp.util.logger import log_error


def get_backup_answer_file() -> Path:
    return Path(current_app.config['FILES_PATH']) / current_app.config['BACKUP_ANSWER_FILE']


def save_answer_backup(answer: ExportedAnswer, token: str) -> Response:
    expected_token = current_app.config['BACKUP_ANSWER_RECEIVE_TOKEN']
    if expected_token is None:
        raise RouteException('BACKUP_ANSWER_RECEIVE_TOKEN not configured.')
    if token != expected_token:
        raise RouteException('Wrong token')
    with filelock.FileLock(f'/tmp/answer_backup'):
        with get_backup_answer_file().open('a') as f:
            f.write(to_json_str(answer) + '\n')
    return ok_response()


def send_answer_backup_if_enabled(a: Answer) -> None:
    if current_app.config['BACKUP_ANSWER_SEND_TOKEN'] is None:
        return
    from timApp.tim_celery import send_answer_backup
    doc_id = a.parsed_task_id.doc_id
    assert doc_id is not None
    doc = DocEntry.find_by_id(doc_id)
    assert doc is not None
    num_users = len(a.users_all)
    if num_users > 1:
        log_error(f'Multiple users not supported for answer backup (answer id {a.id} has {num_users} users)')
        return
    send_answer_backup.delay({
        'email': a.users_all[0].email,
        'content': a.content,
        'valid': a.valid,
        'points': a.points,
        'time': a.answered_on.isoformat(),
        'task': a.task_name,
        'doc': doc.path,
        'host': current_app.config['TIM_HOST'],
    })
