from flask import Flask

from timApp.admin.answer_cli import answer_cli
from timApp.admin.item_cli import item_cli
from timApp.admin.sisu_cli import sisu_cli
from timApp.admin.user_cli import user_cli
from timApp.admin.language_cli import language_cli
from timApp.admin.translationservice_cli import tr_service_cli


def register_clis(app: Flask) -> None:
    for c in [
        answer_cli,
        item_cli,
        sisu_cli,
        user_cli,
        language_cli,
        tr_service_cli,
    ]:
        app.cli.add_command(c)
