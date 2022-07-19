from argparse import ArgumentParser, REMAINDER
from typing import List

from cli.config import get_config
from cli.docker.run import run_compose
from cli.util.errors import CLIError

info = {"help": "Run mailman CLI command"}


class Arguments:
    args: List[str]


def verify_mailman_dev() -> None:
    config = get_config()
    if not config.mailman_dev:
        raise CLIError(
            "Enable local mailman development first (set mailman.is_dev = yes in tim.conf)"
        )


def cmd(args: Arguments) -> None:
    verify_mailman_dev()
    exit(
        run_compose(
            [
                "exec",
                "-e",
                "PYTHONPATH=/usr/mailman_scripts",
                "mailman-core",
                "mailman",
                *args.args,
            ]
        ).returncode
    )


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "args", nargs=REMAINDER, help="Arguments to pass to mailman CLI"
    )
    parser.set_defaults(run=cmd)
