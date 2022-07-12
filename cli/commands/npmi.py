from argparse import ArgumentParser
from typing import Optional

from cli.npm.run import run_npm

info = {
    "help": "Run `npm install` to install TIM dependencies.",
    "description": """
Run `npm install` to install TIM dependencies.

The version of NPM that is used is determined by the OS.
On Windows, the NPM that is installed on the system is used.
On Linux, NPM packaged withing TIM image is used.
""",
}


class Arguments:
    profile: Optional[str]


def cmd(*_) -> None:
    run_npm(["install", "--unsafe-perm"], "timApp")
    # JSRunner has native deps, so it has to be run in container always
    run_npm(["install", "--unsafe-perm"], "timApp/modules/jsrunner/server", True)


def init(parser: ArgumentParser) -> None:
    parser.set_defaults(run=cmd)
