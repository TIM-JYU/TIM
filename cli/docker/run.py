import subprocess
from pathlib import Path
from typing import List, Optional

from cli.config import get_config
from cli.docker.compose import init_compose
from cli.util.errors import CLIError
from cli.util.logging import log_debug

_compose_ok = False


def verify_compose_installed() -> None:
    global _compose_ok
    if _compose_ok:
        return
    try:
        subprocess.run(
            ["docker-compose", "--version"], check=True, stdout=subprocess.PIPE
        )
        _compose_ok = True
    except subprocess.CalledProcessError:
        raise CLIError(
            "docker-compose is not installed; see https://docs.docker.com/compose/install/ to install it"
        )


def get_compose_cmd(
    args: List[str],
    profile: Optional[str] = None,
    with_compose_file: bool = True,
) -> List[str]:
    verify_compose_installed()
    extra_args = []
    if profile:
        extra_args.extend(["--profile", profile])
    if with_compose_file:
        config = get_config()
        init_compose(profile or config.get("compose", "profiles"))
        extra_args.extend(["-f", (Path.cwd() / "docker-compose.yml").as_posix()])
    return ["docker-compose", *extra_args, *args]


def run_compose(
    args: List[str],
    profile: Optional[str] = None,
    with_compose_file: bool = True,
) -> subprocess.CompletedProcess:
    compose_args = get_compose_cmd(args, profile, with_compose_file)
    log_debug(f"run_compose: {compose_args}")
    return subprocess.run(compose_args)
