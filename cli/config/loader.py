import configparser
from pathlib import Path
from typing import Optional, Tuple

from cli.config.config_file import TIMConfig
from cli.config.default_config import DEFAULT_CONFIG
from cli.config.migrations import apply_migrations
from cli.util.logging import log_error

loaded_config: Optional[TIMConfig] = None

config_name = "tim.conf"


def _default_config() -> TIMConfig:
    """
    Load default config
    """
    config = TIMConfig(
        (Path.cwd() / config_name).as_posix(),
        defaults={
            "DIR": Path.cwd().as_posix(),
        },
        allow_no_value=True,
        interpolation=configparser.ExtendedInterpolation(),
    )
    config.load_ext_dict(DEFAULT_CONFIG)
    return config


def _find_config(create_if_not_exist: bool) -> Tuple[TIMConfig, int]:
    # There are few options for config file:
    # 1. No config file, no variables.sh -> Error or return default based on create_if_not_exist
    # 2. No config file, variables.sh exists -> Load default config, return revision 0
    # 3. Config file exists, variables.sh exists -> Load config using the guide below, warn about variables.sh
    # 4. Config file exists -> Load config, return its revision
    # 5. Config file exists, but has no revision coded into it -> Load config, return revision -1

    cwd = Path.cwd()
    config_file_path = cwd / config_name

    old_variables_path = cwd / "variables.sh"
    def_config = _default_config()
    # Cases 1 and 2
    if not config_file_path.exists():
        if not old_variables_path.exists() and not create_if_not_exist:
            log_error(
                "TIM is not initialized. Please run `tim setup` to initialize and configure TIM."
            )
            exit(1)
        if old_variables_path.exists():
            return def_config, 0
    else:
        # Cases 3-5
        def_config.read([config_file_path])
    return def_config, def_config.getint("__meta__", "revision", fallback=-1)


def get_config(create_if_not_exist: bool = False) -> TIMConfig:
    global loaded_config
    if loaded_config:
        return loaded_config
    loaded_config, revision = _find_config(create_if_not_exist)
    apply_migrations(loaded_config, revision)
    return loaded_config
