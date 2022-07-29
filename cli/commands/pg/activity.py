from argparse import ArgumentParser

from cli.commands.pg.query import run_psql

info = {
    "help": "Print active connection information of PostgreSQL database",
}


class Arguments:
    test_db: bool


def run(args: Arguments) -> None:
    run_psql(
        [
            "-c",
            """
SELECT datname as db, pid, usesysid, usename, application_name as app_name,
  client_addr, client_port as port, backend_start,
  xact_start, query_start, state_change, wait_event, state, backend_xid, backend_xmin, query
FROM pg_stat_activity
WHERE application_name <> 'psql'
;
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
