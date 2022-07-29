from argparse import ArgumentParser, REMAINDER
from typing import List

from cli.docker.run import run_compose

info = {"help": "Restart all TIM containers"}


class Arguments:
    args: List[str]


def run(args: Arguments) -> None:
    run_compose(["restart", *args.args])


def init(parser: ArgumentParser) -> None:
    parser.add_argument("args", nargs=REMAINDER, help="Arguments to pass to restart")
