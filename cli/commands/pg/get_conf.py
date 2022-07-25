from argparse import ArgumentParser

from cli.docker.run import run_compose

info = {
    "help": "Get PostgreSQL configuration value",
    "description": """
Get a configuration value from postgresql.conf file.
""",
}


class Arguments:
    test_db: bool
    option_name: str


def run(args: Arguments) -> None:
    service = "postgresql" if not args.test_db else "postgresql-test"
    run_compose(
        [
            "exec",
            service,
            "bash",
            "-c",
            f"grep {args.option_name} /var/lib/postgresql/data/postgresql.conf",
        ]
    )


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--test-db",
        help="If specified, apply the configuration value to the test database",
        dest="test_db",
        action="store_true",
    )
    parser.add_argument(
        "option_name",
        help="Name of the configuration option",
    )
