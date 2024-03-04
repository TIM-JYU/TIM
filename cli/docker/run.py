import os
import subprocess
from pathlib import Path
from typing import List, Optional, Dict, Any

from cli.config import get_config
from cli.docker.compose import init_compose
from cli.util.errors import CLIError
from cli.util.proc import run_cmd

_compose_ok = False
_docker_ok = False
_compose_cmd: List[str] = []


def verify_compose_installed() -> None:
    global _compose_ok
    global _compose_cmd
    if _compose_ok:
        return

    compose_commands = [
        ["docker", "compose"],
        ["docker-compose"],
    ]

    for cmd in compose_commands:
        try:
            run_cmd(
                [*cmd, "version"],
                check=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            _compose_cmd = cmd
            _compose_ok = True
            return
        except (subprocess.CalledProcessError, FileNotFoundError):
            pass

    raise CLIError(
        "docker-compose is not installed; see https://docs.docker.com/compose/install/ to install it"
    )


def verify_docker_installed() -> None:
    global _docker_ok
    if _docker_ok:
        return
    try:
        run_cmd(["docker", "--version"], check=True, stdout=subprocess.PIPE)
        _docker_ok = True
    except (subprocess.CalledProcessError, FileNotFoundError):
        raise CLIError(
            "docker is not installed; see https://docs.docker.com/install/ to install it"
        )


def get_compose_cmd(
    args: List[str],
    profile: Optional[str] = None,
    with_compose_file: bool = True,
    override_profile: bool = True,
) -> List[str]:
    verify_compose_installed()
    extra_args = []
    if profile:
        extra_args.extend(["--profile", profile])
    if with_compose_file:
        config = get_config()
        profile_override = profile or config.profile if override_profile else None
        init_compose(profile_override)
        extra_args.extend(["-f", (Path.cwd() / "docker-compose.yml").as_posix()])
    return [*_compose_cmd, *extra_args, *args]


def run_compose(
    args: List[str],
    profile: Optional[str] = None,
    with_compose_file: bool = True,
    override_profile: bool = True,
    extra_env: Optional[Dict[str, str]] = None,
    capture_output: Optional[bool] = False,
) -> subprocess.CompletedProcess:
    compose_args = get_compose_cmd(args, profile, with_compose_file, override_profile)
    env = dict(os.environ)
    if extra_env:
        env.update(extra_env)
    return run_cmd(compose_args, capture_output=capture_output, env=env)


def run_docker(args: List[str], **kwargs: Any) -> subprocess.CompletedProcess:
    verify_docker_installed()
    return run_cmd(["docker", *args], **kwargs)
