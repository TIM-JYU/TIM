import shlex
import subprocess
import sys
from pathlib import Path
from typing import Callable, List

from cli.config.config_file import TIMConfig
from cli.config.default_config import CURRENT_REVISION
from cli.util.logging import log_error, log_info


def _migrate_variables(config: TIMConfig) -> None:
    """Migrate variables.sh"""

    cwd = Path.cwd()
    variables_path = cwd / "variables.sh"
    if not variables_path.exists():
        return

    # Parse variables.sh and print the resulting environment variables
    res = subprocess.run(
        shlex.split(f'sh -c ". ./variables.sh && env"'),
        stdout=subprocess.PIPE,
        encoding="utf-8",
    )

    variables = {}
    cur_key = ""
    for line in res.stdout.splitlines():
        if line.startswith("_"):
            continue
        if line.startswith("PWD="):
            continue
        if line.startswith("SHLVL="):
            continue
        if "=" in line:
            key, value = line.split("=", 1)
            cur_key = key
            variables[cur_key] = value.strip()
            continue
        if cur_key:
            variables[cur_key] += "\n" + line.strip()

    # Fix up variables
    tim_root = variables["TIM_ROOT"]
    variable_remap = {
        "TIM_HOST": ("tim", "host"),
        "RUN_MAILMAN_DEV": ("mailman", "is_dev"),
        "DRAG_DEV_COMMAND": ("drag", "is_dev"),
        "PG_SHM_SIZE": ("postgresql", "shm_size"),
        "PALI_DEV_COMMAND": ("pali", "is_dev"),
        "FILES_ROOT": ("tim", "files_root"),
        "COMPOSE_PROFILES": ("compose", "profile"),
        "COMPOSE_PROJECT_NAME": ("compose", "project_name"),
        "CONFIG_FILE": ("tim", "config_file"),
        "TEXTFIELD_DEV_COMMAND": ("fields", "is_dev"),
        "PG_MAX_CONNECTIONS": ("postgresql", "max_connections"),
        "CADDY_EXTRA_TIM_CONFIG": ("caddy", "extra_tim_config"),
        "JSRUNNER_DEV_COMMAND": ("jsrunner", "is_dev"),
        "CSPLUGIN_DEV_COMMAND": ("csplugin", "is_dev"),
        "FEEDBACK_DEV_COMMAND": ("feedback", "is_dev"),
        "SVNPLUGIN_DEV_COMMAND": ("showfile", "is_dev"),
        "CADDY_DOMAINS": ("caddy", "domains"),
        "CADDY_EXTRA_CONFIG": ("caddy", "extra_config"),
        "LOG_DIR": ("tim", "log_dir"),
        "IMAGEX_DEV_COMMAND": ("imagex", "is_dev"),
        "STATUS_PAGE_URL": ("tim", "status_page_url"),
        "CSPLUGIN_CASSANDRA_HEAP_NEWSIZE": ("csplugin", "cassandra_heap_newsize"),
        "CSPLUGIN_CASSANDRA_MAX_HEAP_SIZE": ("csplugin", "cassandra_max_heap_size"),
    }

    for key, (section, option) in variable_remap.items():
        if "DEV" in key:
            variable_value = variables.get(key, "0")
            value = "no" if variable_value and variable_value != "1" else "yes"
        else:
            value = variables.get(key, "")
        value = value.replace(tim_root, "${DIR}")
        if value:
            config.set(section, option, value)

    if variables["COMPOSE_PROFILES"] == "prod_multi":
        config.set(
            "caddy", "http_port", f"{variables.get('CADDY_MULTI_PORT', '50000')}"
        )
        config.set("caddy", "https_port", "")
        config.set("caddy", "is_proxied", "yes")
        config.set("compose", "profile", "prod")


MIGRATIONS: List[Callable[[TIMConfig], None]] = [
    _migrate_variables,
]


def apply_migrations(config: TIMConfig, current_revision: int) -> None:
    """
    Apply migrations up to the latest revision
    """

    if current_revision < 0:
        log_error(
            "No revision found in config file. Please run `tim config` to create a new config file."
        )
        sys.exit(1)

    if current_revision > CURRENT_REVISION:
        log_error(
            f"Invalid revision in config file. "
            f"Current available revision is {CURRENT_REVISION} but detect revision is {current_revision}. "
            "Please fix the configuration file or generate a new one with `tim setup` command."
        )
        sys.exit(1)

    if current_revision < len(MIGRATIONS):
        log_info(
            f"Applying automatic migrations to config file (rev {current_revision} -> {len(MIGRATIONS)})..."
        )
        for step, migration in enumerate(
            MIGRATIONS[current_revision:], start=current_revision
        ):
            log_info(
                f"Applying {step} -> {step + 1}: '{migration.__doc__ or migration.__name__}'"
            )
            migration(config)
            config.set("__meta__", "revision", str(step + 1))
            config.save()
