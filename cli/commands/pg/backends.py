from argparse import ArgumentParser

from cli.commands.pg.query import run_psql

info = {
    "help": "Print number of active backends of PostgreSQL database",
}


class Arguments:
    test_db: bool


def cmd(args: Arguments) -> None:
    run_psql(
        [
            "-c",
            """
SELECT datname, numbackends FROM pg_stat_database;
""",
        ],
        args.test_db,
    )


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--test-db",
        help="If specified, apply the configuration value to the test database",
        dest="test_db",
        action="store_true",
    )
    parser.set_defaults(run=cmd)
