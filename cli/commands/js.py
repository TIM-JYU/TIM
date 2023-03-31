from argparse import ArgumentParser, REMAINDER
from typing import List

from cli.commands.npmi import npmi, needs_npmi
from cli.npm.run import run_npm
from cli.util.logging import log_info

info = {"help": "Compile JavaScript files for production mode"}


class Arguments:
    npmi: bool
    target: str
    args: List[str]


def js(run_npmi: bool, extra_args: List[str], target: str = "browser") -> None:
    if run_npmi or needs_npmi():
        log_info("Running `npm install` to install build dependencies.")
        npmi()
    if target in ("server", "browser"):
        run_npm(["run", "buildtools"], "timApp/modules/jsrunner/server", True)
    if target in ("browser",):
        run_npm(["run", "b", "--", *extra_args], "timApp")


def run(args: Arguments) -> None:
    js(args.npmi, args.args, args.target)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--npmi",
        help="Run `npm install` to install TIM dependencies.",
        action="store_true",
    )
    parser.add_argument(
        "--target",
        help="Build only the specified JS (for server-only or for both).",
        choices=["server", "browser"],
        default="browser",
    )
    parser.add_argument(
        "args", nargs=REMAINDER, help="Arguments to pass to docker-compose"
    )
