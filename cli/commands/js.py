from argparse import ArgumentParser, REMAINDER
from typing import List

from cli.npm.run import run_npm

info = {"help": "Compile JavaScript files for production mode"}


class Arguments:
    args: List[str]


def cmd(args: Arguments) -> None:
    run_npm(["run", "buildtools"], "timApp/modules/jsrunner/server", True)
    run_npm(["run", "b", "--", *args.args], "timApp")


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "args", nargs=REMAINDER, help="Arguments to pass to docker-compose"
    )
    parser.set_defaults(run=cmd)
