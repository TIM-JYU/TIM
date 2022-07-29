from argparse import ArgumentParser

from cli.docker.run import run_compose

info = {
    "help": "Set PostgreSQL configuration value",
    "description": """
Set a configuration value for currently running PostgreSQL instance.

The command writes directly to postgresql.conf file.
The configuration will be automatically reset to the default value when the container is restarted.
""",
}


class Arguments:
    test_db: bool
    option_name: str
    option_value: int


def run(args: Arguments) -> None:
    service = "postgresql" if not args.test_db else "postgresql-test"
    run_compose(
        [
            "exec",
            service,
            "bash",
            "-c",
            rf"""
sed -i -e 's/^#\{{0,1\}}{args.option_name} = .*$/{args.option_name} = {args.option_value}/w sed_changes.txt' /var/lib/postgresql/data/postgresql.conf \
  && echo Lines changed in postgresql.conf: \
  && [ -s sed_changes.txt ] || echo \(nothing changed\) \
  && cat sed_changes.txt
""",
        ]
    )
    run_compose(["restart", service])


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
    parser.add_argument(
        "option_value",
        help="Value of the configuration option",
    )
