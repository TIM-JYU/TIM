import datetime
from argparse import ArgumentParser
from pathlib import Path

from cli.commands.pg.backup import pg_backup
from cli.util.logging import log_info, log_debug

info = {
    "help": "Backup the database and rotate the backups to keep only the latest backups",
    "description": """
Backup the database and rotate the backups to keep only the latest backups.
""",
}


class Arguments:
    backup_dir: str
    rotate_days: int


def run(args: Arguments) -> None:
    pg_backup(args.backup_dir)
    log_debug(f"Removing backups older than {args.rotate_days} days")
    p = Path(args.backup_dir)
    remove_older_than = (
        datetime.datetime.now() - datetime.timedelta(days=args.rotate_days)
    ).timestamp()
    for f in p.glob("*.sql.gz"):
        if f.stat().st_mtime < remove_older_than:
            log_debug(f"Removing {f}")
            f.unlink()
    log_info("Backup rotation complete")


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--backup-dir",
        help="Directory to store the backups. Default is <cwd>/pg_backup",
        default=(Path.cwd() / "pg_backup").as_posix(),
        dest="backup_dir",
    )
    parser.add_argument(
        "--rotate-days",
        help="Number of days to keep the backups. Default is 7 days",
        default=7,
        type=int,
        dest="rotate_days",
    )
