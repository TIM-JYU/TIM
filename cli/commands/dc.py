from argparse import ArgumentParser, REMAINDER
from typing import List, Optional

from cli.docker.run import run_compose

info = {
    "help": "Run a docker-compose command on TIM containers",
    "description": """
Run a docker-compose command on TIM containers.
This is a wrapper around docker-compose commands that takes into account extra TIM environment variables.
""",
}


class Arguments:
    profile: Optional[str]
    args: List[str]


def run(args: Arguments) -> None:
    run_compose(args.args, args.profile)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--profile",
        help="TIM run profile override. Default is the same as defined in tim.conf",
        choices=["dev", "prod", "test"],
    )
    parser.add_argument(
        "args", nargs=REMAINDER, help="Arguments to pass to docker-compose"
    )
