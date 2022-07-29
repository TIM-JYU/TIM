from argparse import ArgumentParser
from typing import Optional

from cli.commands.dev.mailman.cli import verify_mailman_dev
from cli.docker.run import run_compose

info = {"help": "Send a message to a mailing list"}


class Arguments:
    list: str
    message: Optional[str]


def run(args: Arguments) -> None:
    verify_mailman_dev()
    cmd_args = [
        "exec",
        "-e",
        "PYTHONPATH=/usr/mailman_scripts",
        "mailman-core",
        "mailman",
        "shell",
        "--run",
        "send_message",
        "-l",
        args.list,
    ]
    if args.message:
        cmd_args.append(args.message)
    exit(run_compose(cmd_args).returncode)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--message",
        help="Message to send WITH the necessary MIME headers. "
        "If not specified, an editor will open to specify the message",
    )
    parser.add_argument(
        "list",
        help="List to send message to",
    )
