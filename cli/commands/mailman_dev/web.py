from argparse import ArgumentParser, REMAINDER
from typing import List

from cli.commands.mailman_dev.cli import verify_mailman_dev
from cli.docker.run import run_compose

info = {"help": "Run mailman-web manage.py command"}


class Arguments:
    args: List[str]


def cmd(args: Arguments) -> None:
    verify_mailman_dev()
    exit(
        run_compose(
            [
                "exec",
                "mailman-web",
                "python3",
                "manage.py",
                *args.args,
            ]
        ).returncode
    )


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "args", nargs=REMAINDER, help="Arguments to pass to mailman-web manage.py"
    )
    parser.set_defaults(run=cmd)
