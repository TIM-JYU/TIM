from typing import List, Any

from cli.docker.run import run_compose

info = {"help": "(Re)create all containers and start TIM"}


def up() -> None:
    run_compose(["up", "-d", "--remove-orphans"])


def run(*_: List[Any]) -> None:
    up()
