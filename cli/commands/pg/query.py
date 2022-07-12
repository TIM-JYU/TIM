from argparse import ArgumentParser, REMAINDER
from pathlib import Path
from typing import List

from cli.config import get_config
from cli.docker.run import run_compose

info = {
    "help": "Run PSQL query on the database",
    "description": """
Run a query on the database using the PSQL command.
""",
}


class Arguments:
    test_db: bool
    args: List[str]


def run_psql(args: List[str], test_db: bool) -> None:
    config = get_config()
    cwd = Path.cwd()
    db_name = config.get("compose", "project_name") if not test_db else "tim-test"
    service = "postgresql" if not test_db else "postgresql-test"
    run_compose(
        [
            "run",
            "--rm",
            "-v",
            f"{cwd / 'pg_backup'}:/backup",
            "-v",
            f"{cwd}:/workdir:ro",
            "--workdir",
            "/workdir",
            "postgresql",
            "psql",
            "-P",
            "pager=off",
            "-h",
            service,
            "-p",
            "5432",
            "-d",
            db_name,
            "-U",
            "postgres",
            *args,
        ]
    )


def cmd(args: Arguments) -> None:
    run_psql(args.args, args.test_db)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--test-db",
        help="If specified, apply the configuration value to the test database",
        dest="test_db",
        action="store_true",
    )
    parser.add_argument("args", nargs=REMAINDER, help="Arguments to pass to psql")
    parser.set_defaults(run=cmd)
