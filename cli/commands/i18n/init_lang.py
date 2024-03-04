from argparse import ArgumentParser

from cli.commands.i18n.extract import run_pybabel
from cli.docker.run import run_compose
from cli.util.logging import log_info

info = {
    "help": "Initialize a new translation catalog for server messages",
}


class Arguments:
    language: str


def init_catalog(lang: str) -> None:
    log_info("Initializing translation catalog")
    run_pybabel(["init", "-i", "messages.pot", "-d", "i18n", "-l", f"{lang}"])


def run(args: Arguments) -> None:
    if not args.language:
        log_info("No language specified.")
        log_info("Please provide a CLDR compliant language code.")
        log_info(
            "See https://cldr.unicode.org/index/cldr-spec/picking-the-right-language-code for details."
        )
        return

    if not check_locale(args.language):
        log_info(f"Unknown language code: {args.language}")
        return

    init_catalog(args.language)


def check_locale(lang: str) -> bool:
    locales = run_compose(
        [
            "run",
            "--rm",
            "--workdir",
            "/service/timApp",
            "--no-deps",
            "tim",
            "pybabel",
            "--list-locales",
        ],
        capture_output=True,
    ).stdout.decode("utf-8")
    codes = list(map(lambda x: x.partition(" ")[0], locales.splitlines()))
    return lang in codes


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "language",
        help="The language for which to initialize a new translation catalog.",
        nargs="?",
    )
