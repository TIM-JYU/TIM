from argparse import ArgumentParser, Namespace

info = {"help": "(Re)creates all containers and starts TIM"}


def cmd(args: Namespace) -> None:
    print(f"hello world {args}")


def init(parser: ArgumentParser) -> None:
    parser.add_argument("--name", help="Name to print")
    parser.set_defaults(run=cmd)
