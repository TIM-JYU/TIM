import logging
import os
import sys
from typing import Dict

# Needed to force color codes to work in Windows
os.system("")


class ColoredFormatter(logging.Formatter):
    grey = "\x1b[0;90m"
    green = "\x1b[1;32m"
    yellow = "\x1b[1;33m"
    red = "\x1b[1;31m"
    purple = "\x1b[1;35m"
    blue = "\x1b[1;34m"
    light_blue = "\x1b[1;36m"
    reset = "\x1b[0m"
    blink_red = "\x1b[5m\x1b[1;31m"
    format_str = "[{}%(levelname)s{}] %(message)s"

    FORMATS: Dict[int, str] = {
        logging.DEBUG: format_str.format(grey, reset),
        logging.INFO: format_str.format(blue, reset),
        logging.WARNING: format_str.format(yellow, reset),
        logging.ERROR: format_str.format(red, reset),
        logging.CRITICAL: format_str.format(red, reset),
    }

    def format(self, record: logging.LogRecord) -> str:
        fmt = self.FORMATS[record.levelno]
        formatter = logging.Formatter(fmt)
        return formatter.format(record)


logging.root.setLevel(logging.INFO)
cli_logger = logging.getLogger("cli")
ch = logging.StreamHandler(sys.stdout)
ch.setLevel(logging.INFO)
ch.setFormatter(ColoredFormatter())
cli_logger.addHandler(ch)


def enable_verbose() -> None:
    cli_logger.setLevel(logging.DEBUG)
    ch.setLevel(logging.DEBUG)


def log_error(msg: str) -> None:
    cli_logger.error(msg)


def log_info(msg: str) -> None:
    cli_logger.info(msg)


def log_warning(msg: str) -> None:
    cli_logger.warning(msg)


def log_debug(msg: str) -> None:
    cli_logger.debug(msg)
