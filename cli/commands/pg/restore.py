import gzip
from argparse import ArgumentParser
from pathlib import Path
from subprocess import Popen, PIPE

from cli.docker.run import get_compose_cmd
from cli.util.errors import CLIError
from cli.util.iter import iter_chunked
from cli.util.logging import log_info, log_warning, log_debug

info = {
    "help": "Restore the database from a gzipped SQL dump",
    "description": """
Restores the entire PostgreSQL database from a gzipped SQL dump.

The operation will overwrite the existing database.
""",
}


class Arguments:
    input: str


def cmd(args: Arguments) -> None:
    input_file = Path(args.input).resolve()
    if not input_file.exists() or not input_file.is_file():
        raise CLIError(f"{input_file} is not a file")

    log_warning(
        f"You are about to restore PostgreSQL database from {input_file}. "
        "The current database will be overwritten."
    )
    answer = input("Type 'yes' to continue: ")
    if answer != "yes":
        log_info("Aborted")
        return

    log_info(f"Restoring PostgreSQL database from {input_file}")
    compose_cmd = get_compose_cmd(
        [
            "exec",
            "-T",
            "postgresql",
            "psql",
            "-h",
            "localhost",
            "-p",
            "5432",
            "-U",
            "postgres",
        ]
    )
    with Popen(compose_cmd, stdin=PIPE) as proc:
        assert proc.stdin is not None
        log_debug(f"Opening gzip {input_file}")
        with gzip.open(input_file, "rb") as f:
            log_debug("Writing gzip to stdin")
            for chunk in iter_chunked(f):
                proc.stdin.write(chunk)
                proc.stdin.flush()
        log_debug("Closing stdin and waiting")
        proc.stdin.close()
        proc.wait()
        if proc.returncode != 0:
            raise CLIError(f"Failed to restore the database", proc.returncode)
    log_info(f"Restore complete")


def init(parser: ArgumentParser) -> None:
    parser.add_argument("input", help="Input SQL file")
    parser.set_defaults(run=cmd)
