from argparse import ArgumentParser
from subprocess import PIPE
from typing import Optional, Set

from cli.commands.i18n.compile import compile_translations
from cli.commands.js import js
from cli.commands.rust import build_rust
from cli.commands.up import up
from cli.config import get_config
from cli.docker.run import run_compose, get_compose_cmd
from cli.util.logging import log_info, log_warning, log_error
from cli.util.proc import run_cmd

info = {
    "help": "Update the running TIM instance using a specific update strategy",
    "description": """
Updates the currently running TIM instance to run the currently active commit of the TIM repository.

The command will try to gracefully handle upgrading according to the selected upgrade strategy.
""",
}


class Arguments:
    strategy: Optional[str]
    yes: bool
    down: bool


def back() -> None:
    """Update TIM server by restarting tim service. Does not drop requests."""
    gunicorn_pid_path = "/var/run/gunicorn/gunicorn.pid"
    log_info("Sending restart signal to Gunicorn")
    run_compose(
        [
            "exec",
            "tim",
            "bash",
            "-c",
            f"[ -f {gunicorn_pid_path} ] && kill -HUP $(cat {gunicorn_pid_path}) || echo 'Gunicorn is not running, nothing to do!'",
        ]
    )


def front() -> None:
    """Update TIM frontend (JS), restarts tim service. Does not drop requests."""
    log_info("Rebuilding JS")
    js(True, [])
    back()


def full_light() -> None:
    """Update backend and frontend, restart tim service. Does not drop requests."""
    # For now it's just an alias for back()
    front()


def _stop_services(except_services: Set[str]) -> None:
    log_info("Stopping TIM first")
    run_compose(["stop", "tim"])
    services_cmd = get_compose_cmd(["ps", "--services"])
    res = run_cmd(services_cmd, stdout=PIPE, encoding="utf-8")
    services = set(res.stdout.splitlines())
    services -= except_services
    log_info(f"Stopping services")
    run_compose(["stop", *services])


def _migrate_db() -> None:
    log_info("Updating TIM database")
    run_compose(
        [
            "run",
            "--rm",
            "--no-deps",
            "--workdir",
            "/service/timApp",
            "tim",
            "flask",
            "db",
            "upgrade",
        ]
    )


def db() -> None:
    """Update the TIM database and backend. Causes downtime."""
    _stop_services({"postgresql", "caddy"})
    _migrate_db()

    log_info("Starting everything up")
    up()


def all() -> None:
    """Update Docker images, database, frontend and backend. Causes downtime."""
    _stop_services({"caddy"})

    log_info("Updating TIM images")
    run_compose(["pull"])

    log_info("Updating TIM database")
    run_compose(["up", "-d", "postgresql"])
    _migrate_db()

    log_info("Rebuilding Rust dependencies")
    build_rust()

    log_info("Compiling translations")
    compile_translations()

    log_info("Updating TIM frontend")
    js(True, [])

    log_info("Starting everything up")
    up()


update_commands = [
    back,
    front,
    full_light,
    db,
    all,
]


def run(args: Arguments) -> None:
    if not args.strategy:
        log_info("Use `./tim update <strategy>` to use a specific strategy.")
        log_info("Available update strategies:")
        # add padding to names to align them
        max_name_length = max(len(command.__name__) for command in update_commands)
        for command in update_commands:
            docstring = command.__doc__ or ""
            log_info(f"  {command.__name__:<{max_name_length}} - {docstring.strip()}")
        return

    config = get_config()

    command = next(
        command for command in update_commands if command.__name__ == args.strategy
    )
    docstring = command.__doc__ or ""

    causes_downtime = "downtime" in docstring.lower()
    skip_verify = (
        config.profile == "dev"
        or (not causes_downtime and args.yes)
        or (causes_downtime and args.yes and args.down)
    )
    if not skip_verify:
        log_info(
            f"The following update strategy will be used: {command.__name__} ({docstring})"
        )
        if causes_downtime or args.down:
            log_warning(
                "This strategy will cause downtime. Make sure you have informed the users first!"
            )
        if input("Are you sure you want to continue? [y/N] ").lower() != "y":
            log_error("Aborting")
            return

    if not causes_downtime and args.down:
        log_warning(
            "This strategy does not cause downtime, but downtime was requested. Stopping services."
        )
        _stop_services({"caddy"})

    command()

    if not causes_downtime and args.down:
        log_info("Starting everything up")
        up()


def init(parser: ArgumentParser) -> None:
    command_names = [command.__name__ for command in update_commands]
    parser.add_argument(
        "--yes",
        help="Skip confirmation prompts for light strategies. For downtime strategies, pass `--yes --down`",
        action="store_true",
    )
    parser.add_argument(
        "--down",
        help="Stops all services except for caddy to display downtime messages.",
        action="store_true",
    )
    parser.add_argument(
        "strategy",
        help="Update strategy to use. If not specified, all available strategies are listed.",
        choices=command_names,
        nargs="?",
    )
