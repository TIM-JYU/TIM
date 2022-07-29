import datetime
import os
import platform
import shutil
import subprocess
from argparse import ArgumentParser
from pathlib import Path
from subprocess import PIPE
from typing import Optional

from cli.config import get_config
from cli.docker.run import run_docker
from cli.util.errors import CLIError
from cli.util.logging import log_info, log_debug
from cli.util.proc import run_cmd

info = {"help": "Profile Python code using py-spy"}


class Arguments:
    service: str


def is_in_wsl() -> bool:
    return "microsoft-standard" in platform.uname().release


def get_pyspy() -> str:
    err = "Could not find py-spy. Install py-spy from https://github.com/benfred/py-spy and try again"
    try:
        py_spy_path = shutil.which("py-spy")
        if not py_spy_path:
            raise CLIError(err)
        run_cmd([py_spy_path, "--version"], check=True, stdout=PIPE)
        return py_spy_path
    except (subprocess.CalledProcessError, FileNotFoundError):
        raise CLIError(err)


def run(args: Arguments) -> None:
    if platform.system() == "Windows":
        raise CLIError("Profiling is not supported on Windows")
    elif is_in_wsl():
        raise CLIError("Profiling is not supported in Docker for Windows")

    if os.getuid() != 0:
        raise CLIError("You must run profiling command as root")

    py_spy_path = get_pyspy()
    log_debug(f"py-spy path: {py_spy_path}")
    config = get_config()
    normalized_service = args.service.replace("-", "_")
    container_name = f"{config.project_name}_{normalized_service}_1"
    p = run_docker(["ps", "-q"], stdout=PIPE, encoding="utf-8")
    found_pid: Optional[str] = None
    for container in p.stdout.splitlines():
        container = container.strip()
        inspect_result = run_docker(
            ["inspect", "--format", "{{.State.Pid}},{{.Name}}", container],
            stdout=PIPE,
            encoding="utf-8",
        )
        pid, name = inspect_result.stdout.strip().split(",")
        # Normalize name to account for Docker Compose v2
        name = name.replace("/", "").replace("-", "_")
        log_debug(f"Inspect container {container}: {pid}, {name}")
        if name == container_name:
            found_pid = pid
            break

    if not found_pid:
        raise CLIError(
            f"Could not find active command PID for container {container_name}. Check that the service is running."
        )

    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    flamegraph_name = f"pyspy_{args.service}_{timestamp}.svg"
    flamegraph_path = Path.cwd() / "timApp" / "static" / "profiling" / flamegraph_name

    cli_args = [
        "sudo",
        py_spy_path,
        "record",
        "-p",
        found_pid,
        "-o",
        flamegraph_path.as_posix(),
        "--subprocesses",
    ]

    log_debug(f"py-spy args: {cli_args}")
    try:
        p = run_cmd(cli_args)
        ret_code = p.returncode
    except KeyboardInterrupt:
        ret_code = 0
    if ret_code == 0:
        log_info(f"Open flamegraph: {config.host}/static/profiling/{flamegraph_name}")
    exit(ret_code)


def init(parser: ArgumentParser) -> None:
    parser.add_argument("service", help="Service to profile")
