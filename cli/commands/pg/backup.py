import datetime
import gzip
from argparse import ArgumentParser
from pathlib import Path
from subprocess import Popen, PIPE

from cli.docker.run import get_compose_cmd
from cli.util.errors import CLIError
from cli.util.iter import iter_chunked
from cli.util.logging import log_info

info = {
    "help": "Backup the database",
    "description": """
Backs up the entire PostgreSQL database to a file.

The backup is a full SQL dump of the database, compressed with gzip.
""",
}


class Arguments:
    output: str


def pg_backup(output: str) -> None:
    output_dir = Path(output).resolve()
    if not output_dir.exists() or not output_dir.is_dir():
        raise CLIError(f"{output_dir} is not a directory")
    filename = datetime.datetime.strftime(datetime.datetime.now(), "%d-%m-%Y_%H_%M_%S")
    output_file = output_dir / f"dump_{filename}.sql.gz"
    log_info(f"Backing up database to {output_file}")
    compose_cmd = get_compose_cmd(
        [
            "exec",
            "-T",
            "postgresql",
            "pg_dumpall",
            "--clean",
            "-h",
            "localhost",
            "-p",
            "5432",
            "-U",
            "postgres",
        ]
    )
    with Popen(compose_cmd, stdout=PIPE) as proc:
        assert proc.stdout is not None
        with gzip.open(output_file, "wb") as f:
            for chunk in iter_chunked(proc.stdout):
                f.write(chunk)
        proc.wait()
        if proc.returncode != 0:
            raise CLIError(f"Failed to back up database", proc.returncode)
    log_info(f"Backup complete")


def run(args: Arguments) -> None:
    pg_backup(args.output)


def init(parser: ArgumentParser) -> None:
    parser.add_argument("output", help="Output directory of the backup file")
