from pathlib import Path
from typing import Any

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

NODE_MODULES = [
    Path.cwd() / "timApp" / "node_modules",
    Path.cwd() / "timApp" / "modules" / "jsrunner" / "server" / "node_modules",
]


def needs_npmi() -> bool:
    return any(not modules_dir.exists() for modules_dir in NODE_MODULES)


def npmi() -> None:
    # Ensure all module folders exist, see:
    # https://github.com/npm/cli/issues/3208
    # https://github.com/npm/cli/issues/2011
    # https://github.com/TIM-JYU/tim-private/issues/41
    for modules_dir in NODE_MODULES:
        modules_dir.mkdir(parents=True, exist_ok=True)

    run_npm(["install"], "timApp")
    # JSRunner has native deps, so it has to be run in container always
    run_npm(["install"], "timApp/modules/jsrunner/server", True)


def run(*_: Any) -> None:
    npmi()
