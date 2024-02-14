from argparse import ArgumentParser

from cli.commands.i18n.extract import run_pybabel
from cli.util.logging import log_info

info = {
    "help": "Compile server translations",
}


class Arguments:
    pass


def compile_translations() -> None:
    log_info("Compiling translations")
    run_pybabel(["compile", "-d", "i18n"])


def run(args: Arguments) -> None:
    compile_translations()


def init(parser: ArgumentParser) -> None:
    pass
