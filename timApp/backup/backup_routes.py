from flask import Response

from timApp.answer.backup import save_answer_backup
from timApp.answer.exportedanswer import ExportedAnswer
from timApp.tim_app import csrf
from timApp.util.flask.typedblueprint import TypedBlueprint

backup = TypedBlueprint(
    'backup',
    __name__,
    url_prefix='/backup/',
)


@backup.post('answer')
@csrf.exempt
def receive_answer_backup(answer: ExportedAnswer, token: str) -> Response:
    return save_answer_backup(answer, token)
