from timApp.admin.answer_cli import answer_cli
from timApp.admin.item_cli import item_cli
from timApp.admin.sisu_cli import sisu_cli
from timApp.admin.user_cli import user_cli


def register_clis(app):
    for c in [
        answer_cli,
        item_cli,
        sisu_cli,
        user_cli,
    ]:
        app.cli.add_command(c)
