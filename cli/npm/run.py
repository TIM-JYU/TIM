import platform
import subprocess
from pathlib import Path
from typing import List, Optional

from cli.docker.run import run_compose
from cli.util.errors import CLIError
from cli.util.logging import log_debug
from cli.util.proc import run_cmd

MAX_NPM_MAJOR_VERSION = 6

_npm_ok = False


def verify_npm() -> None:
    global _npm_ok
    if _npm_ok:
        return
    try:
        res = run_cmd(
            ["npm", "--version"],
            shell=True,
            check=True,
            stdout=subprocess.PIPE,
            encoding="utf-8",
        )
        version = res.stdout.strip()
        version_parts = [int(x) for x in version.split(".")]
        if version_parts[0] > MAX_NPM_MAJOR_VERSION:
            raise CLIError(
                f"TIM requires NPM version {MAX_NPM_MAJOR_VERSION}.x to run locally. "
                f"Run `npm i -g npm@{MAX_NPM_MAJOR_VERSION}` to install it."
            )
    except FileNotFoundError:
        raise CLIError(
            "npm is not installed; see https://docs.npmjs.com/getting-started/installing-node/ to install it"
        )


def run_npm(
    args: List[str],
    workdir: str,
    run_in_container: Optional[bool] = None,
) -> None:
    if run_in_container is None:
        log_debug(f"OS identifier: {platform.system()}")
        run_in_container = platform.system() != "Windows"
    log_debug(f"Running npm with args: {args}; running in docker: {run_in_container}")
    if not run_in_container:
        verify_npm()
        run_cmd(
            ["npm", *args],
            shell=True,
            cwd=(Path.cwd() / workdir).as_posix(),
        )
    else:
        run_compose(
            [
                "run",
                "--rm",
                "--no-deps",
                "--workdir",
                f"/service/{workdir}",
                "tim",
                "npm",
                *args,
            ]
        )
