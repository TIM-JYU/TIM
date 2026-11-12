import platform
import subprocess
from pathlib import Path
from shutil import which
from typing import List, Optional

from cli.docker.run import run_compose
from cli.util.errors import CLIError
from cli.util.logging import log_debug
from cli.util.proc import run_cmd

# Oldest npm that can be used for TIM's main packages when running npm locally (i.e. not in the TIM container).
# The TIM container has Node.js 24 with npm 11.
MIN_NPM_MAJOR_VERSION = 10
# npm that is installed by `./tim setup` if the local npm is too old
DEFAULT_NPM_MAJOR_VERSION = 11

# JSRunner server is run from the "angjsrunner" copy that works with Node.js 24.
# The original server in timApp/modules/jsrunner/server needs Node.js 18 and is kept unchanged.
JSRUNNER_SERVER_DIR = "timApp/modules/angjsrunner/server"

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
        _npm_version_ok = version_parts[0] >= MIN_NPM_MAJOR_VERSION
        if not _npm_version_ok and assert_version:
            raise CLIError(
                f"TIM requires NPM version {MIN_NPM_MAJOR_VERSION} or newer to run locally (found {version}). "
                f"Install Node.js 24 or run `npm i -g npm@{DEFAULT_NPM_MAJOR_VERSION}` to install it."
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
    """
    Runs npm in the given directory.

    :param args: Arguments to npm.
    :param workdir: Directory in which to run npm, relative to TIM root.
    :param run_in_container: Whether to run npm in a container (default: everywhere except on Windows).
    """
    if run_in_container is None:
        log_debug(f"OS identifier: {platform.system()}")
        run_in_container = platform.system() != "Windows"
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


def run_jsrunner_npm(args: List[str]) -> None:
    """
    Runs npm for JSRunner server.

    JSRunner has native dependencies, so npm is always run in a container.
    The container is the JSRunner's own one (and not TIM's) so that the dependencies are
    installed and built in the same container that runs JSRunner.
    """
    log_debug(f"Running JSRunner npm with args: {args}")
    run_compose(
        [
            "run",
            "-T",
            "--rm",
            "--no-deps",
            "--workdir",
            f"/{JSRUNNER_SERVER_DIR}",
            # The container's root file system is read-only, so tools must cache to /tmp
            "-e",
            "HOME=/tmp",
            "jsrunner",
            "npm",
            *args,
        ]
    )
