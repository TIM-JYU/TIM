import shutil
from pathlib import Path
from typing import Any

from cli.config import get_config
from cli.docker.run import run_docker
from cli.docker.service_variables import csplugin_target, csplugin_image_tag
from cli.util.logging import log_info

info = {
    "help": "Build TIM's Rust dependencies.",
    "description": """
Build TIM's Rust dependencies.

Build is done in a Docker container, so you need to have Docker installed.
""",
}

TIM_RUST_PATH = Path.cwd() / "tim_rust"


def build_rust() -> None:
    config = get_config()
    csplugin_image = f"{config.images_repository}/cs3:{csplugin_target(config.profile)}-{csplugin_image_tag()}"
    log_info("Building Rust dependencies")
    run_docker(
        [
            "run",
            "--rm",
            "--user",
            "0",
            "-w",
            "/app",
            "-v",
            f"{TIM_RUST_PATH.absolute()}/:/app/",
            "--tmpfs",
            "/cargo/registry",
            csplugin_image,
            "cargo",
            "build",
            "--release",
        ],
        cwd=TIM_RUST_PATH,
    )
    libtim_path = TIM_RUST_PATH / "target" / "release" / "libtim_rust.so"
    libtim_path_target = TIM_RUST_PATH / "python" / "tim_rust.so"
    shutil.copyfile(libtim_path, libtim_path_target)


def run(*_: Any) -> None:
    build_rust()
