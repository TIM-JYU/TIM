from argparse import ArgumentParser
from subprocess import CompletedProcess
from typing import List, Optional

from cli.docker.run import run_compose
from cli.npm.run import run_npm
from cli.util.logging import log_info

info = {
    "help": "Extract translation strings from the source code",
}


class Arguments:
    pass


def run_pybabel(
    cmds: List[str], capture_output: Optional[bool] = None
) -> CompletedProcess:
    return run_compose(
        [
            "run",
            "--rm",
            "--workdir",
            "/service/timApp",
            "--no-deps",
            "tim",
            "pybabel",
            *cmds,
        ],
        capture_output=capture_output if capture_output else None,
    )


def run(args: Arguments) -> None:
    log_info("Extracting translation strings from server code")
    run_pybabel(
        [
            "extract",
            "-F",
            "babel.cfg",
            "-o",
            "messages.pot",
            ".",
        ]
    )
    log_info("Merging server translations")
    run_pybabel(
        [
            "update",
            "-i",
            "messages.pot",
            "-d",
            "i18n",
        ]
    )
    log_info("Extracting browser translation strings")
    run_npm(["run", "extract-i18n"], "timApp")


def init(parser: ArgumentParser) -> None:
    pass
