from argparse import ArgumentParser

from cli.docker.run import run_compose

info = {"help": "(Re)create all containers and start TIM"}


def cmd(*_) -> None:
    run_compose(["up", "-d", "--remove-orphans"])


def init(parser: ArgumentParser) -> None:
    parser.set_defaults(run=cmd)
