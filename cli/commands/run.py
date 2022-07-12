from argparse import ArgumentParser, REMAINDER
from typing import List, Optional

from cli.docker.run import run_compose

info = {
    "help": "Run a command in a TIM container",
    "description": """
Run a command in a TIM container and dispose of it.
This is a wrapper around `docker-compose run` that takes into account extra TIM environment variables.
""",
}


class Arguments:
    profile: Optional[str]
    container: str
    no_deps: bool
    workdir: str
    args: List[str]


def cmd(args: Arguments) -> None:
    run_args = ["run", "--rm"]
    if args.no_deps:
        run_args.append("--no-deps")
    run_args.extend(["--workdir", args.workdir])
    run_args.append(args.container)
    run_args.extend(args.args)

    run_compose(run_args, args.profile)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--profile",
        help="TIM run profile override. Default is the same as defined in tim.conf",
        choices=["dev", "prod", "test"],
    )
    parser.add_argument(
        "--container",
        help="Container to run the command in. Default is the main `tim` container.",
        default="tim",
    )
    parser.add_argument(
        "--workdir",
        help="Working directory inside the container. Default is `/service/timApp`.",
        default="/service/timApp",
    )
    parser.add_argument(
        "--no-deps",
        help="Don't launch the dependencies of the container. Default is to launch them.",
        action="store_true",
        dest="no_deps",
    )
    parser.add_argument(
        "args", nargs=REMAINDER, help="Arguments to pass to the container"
    )
    parser.set_defaults(run=cmd)
