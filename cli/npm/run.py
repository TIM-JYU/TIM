import platform
import subprocess
from pathlib import Path
from shutil import which
from typing import List, Optional

from cli.config import get_config
from cli.config.config_file import IdeProfile
from cli.docker.run import run_compose
from cli.util.errors import CLIError
from cli.util.logging import log_debug
from cli.util.proc import run_cmd

MAX_NPM_MAJOR_VERSION = 6

_npm_ok = False
_npm_version_ok = False
_npm_cmd: List[str] = []


def reset_npm_version() -> None:
    global _npm_version_ok
    global _npm_ok
    _npm_version_ok = False
    _npm_ok = False


def verify_npm(assert_version: bool = True) -> bool:
    global _npm_ok
    global _npm_version_ok
    global _npm_cmd
    if _npm_ok:
        return _npm_version_ok
    try:
        # On Windows (and on Linux with nvm), npm is run via proxy cmd (and not executable)
        # Therefore, we need to use the "which" command to find the command
        npm_cmd = which("npm")
        if not npm_cmd:
            raise FileNotFoundError()
        res = run_cmd(
            [npm_cmd, "--version"],
            check=True,
            stdout=subprocess.PIPE,
            encoding="utf-8",
        )
        version = res.stdout.strip()
        version_parts = [int(x) for x in version.split(".")]
        _npm_version_ok = version_parts[0] <= MAX_NPM_MAJOR_VERSION
        if not _npm_version_ok and assert_version:
            raise CLIError(
                f"TIM requires NPM version {MAX_NPM_MAJOR_VERSION}.x to run locally. "
                f"Run `npm i -g npm@{MAX_NPM_MAJOR_VERSION}` to install it."
            )
        _npm_ok = True
        _npm_cmd = [npm_cmd]
        return _npm_version_ok
    except (subprocess.CalledProcessError, FileNotFoundError):
        raise CLIError(
            "npm not found; see https://docs.npmjs.com/getting-started/installing-node/ to install it"
        )


def run_npm(
    args: List[str],
    workdir: str,
    run_in_container: Optional[bool] = None,
) -> None:
    if run_in_container is None:
        config = get_config()
        is_vscode_dev = config.ide_profile == IdeProfile.VSCode
        log_debug(f"OS identifier: {platform.system()}")
        run_in_container = is_vscode_dev or platform.system() != "Windows"
    log_debug(f"Running npm with args: {args}; running in docker: {run_in_container}")
    if not run_in_container:
        verify_npm()
        run_cmd(
            [*_npm_cmd, *args],
            cwd=(Path.cwd() / workdir).as_posix(),
        )
    else:
        run_compose(
            [
                "run",
                "-T",
                "--rm",
                "--no-deps",
                "--workdir",
                f"/service/{workdir}",
                "tim",
                "npm",
                *args,
            ]
        )
