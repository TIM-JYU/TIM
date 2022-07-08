import os
import os.path
import subprocess
from argparse import ArgumentParser, REMAINDER
from typing import List

from cli.config import get_config
from cli.docker.compose import init_compose

info = {
    "help": "Run a docker-compose command on TIM containers",
    "description": """
Run a docker-compose command on TIM containers.
This is a wrapper around docker-compose commands that takes into account extra TIM environment variables.
""",
}


class Arguments:
    profile: str
    args: List[str]


def cmd(args: Arguments) -> None:
    config = get_config()
    init_compose(args.profile or config.get("compose", "profiles"))
    cwd = os.getcwd()
    extra_args = ["-f", os.path.join(cwd, "docker-compose.yml")]
    subprocess.run(["docker-compose", *extra_args, *args.args])


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--profile",
        help="TIM run profile override. Default is the same as defined in tim.conf",
        choices=["dev", "prod", "test"],
    )
    parser.add_argument(
        "args", nargs=REMAINDER, help="Arguments to pass to docker-compose"
    )
    parser.set_defaults(run=cmd)
