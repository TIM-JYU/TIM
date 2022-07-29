import datetime
import gzip
import subprocess
from argparse import ArgumentParser
from pathlib import Path
from subprocess import Popen, PIPE
from typing import List

from cli.docker.run import get_compose_cmd
from cli.util.errors import CLIError
from cli.util.iter import iter_chunked
from cli.util.logging import log_info, log_debug
from cli.util.proc import sh_join

info = {
    "help": "Backup the database",
    "description": """
Backs up the entire PostgreSQL database to a file.

The backup is a full SQL dump of the database, compressed with gzip.
""",
}


class Arguments:
    shell: bool
    output: str


def backup_python(output_file: Path, compose_cmd: List[str]) -> None:
    with Popen(compose_cmd, stdout=PIPE) as proc:
        assert proc.stdout is not None
        with gzip.open(output_file, "wb") as f:
            for chunk in iter_chunked(proc.stdout):
                f.write(chunk)
        proc.wait()
        if proc.returncode != 0:
            raise CLIError(f"Failed to back up database", proc.returncode)


def backup_shell(output_file: Path, compose_cmd: List[str]) -> None:
    command = f"{sh_join(compose_cmd)} | gzip > {output_file.as_posix()}"
    log_debug(f"Running backup command: {command}")
    res = subprocess.run(command, shell=True)
    if res.returncode != 0:
        raise CLIError(f"Failed to back up database", res.returncode)


def pg_backup(output: str, shell: bool = False) -> None:
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
    if shell:
        backup_shell(output_file, compose_cmd)
    else:
        backup_python(output_file, compose_cmd)
    log_info(f"Backup complete")


def run(args: Arguments) -> None:
    pg_backup(args.output, args.shell)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--shell",
        help="Whether to use shell and native gzip command to write the command."
        "This assumes that `gzip` command is installed and available.",
        action="store_true",
    )
    parser.add_argument("output", help="Output directory of the backup file")
