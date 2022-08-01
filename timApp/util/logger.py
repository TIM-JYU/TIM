"""Sets up logging for the application."""
import logging
import os
import sys
import time
from datetime import datetime
from logging.handlers import TimedRotatingFileHandler
from pathlib import Path
from typing import Any

import isodate
from flask import Flask

tim_logger = logging.getLogger("tim")
tim_logger.setLevel(logging.DEBUG)

wz = logging.getLogger("werkzeug")
wz.setLevel(logging.ERROR)


class StampedTimedRotatingFileHandler(TimedRotatingFileHandler):
    """
    Extended TimedRotatingFileHandler that tracks the last rotation time via an additional .log_timestamp file.
    """

    def __init__(self, filename: str, *args: Any, **kwargs: Any) -> None:
        self.log_timestamp_file = None
        _, ext = os.path.splitext(filename)
        super().__init__(filename, *args, **kwargs)
        base_dir = Path(filename).parent
        self.log_timestamp_file = base_dir / ".log_timestamp"
        t = self._get_log_time()
        self.rolloverAt = self.computeRollover(t)

    def _get_log_time(self) -> int:
        assert self.log_timestamp_file is not None
        if self.log_timestamp_file.exists():
            # noinspection PyBroadException
            try:
                return int(
                    isodate.parse_datetime(
                        self.log_timestamp_file.read_text()
                    ).timestamp()
                )
            except Exception:
                pass
        return int(time.time())

    # noinspection PyPep8Naming
    def computeRollover(self, currentTime: int) -> int:
        result = super().computeRollover(currentTime)
        if self.log_timestamp_file:
            self.log_timestamp_file.write_text(
                isodate.datetime_isoformat(datetime.fromtimestamp(currentTime))
            )
        return result


def setup_logging(app: Flask) -> None:
    if not app.config["TESTING"]:
        logging.getLogger("alembic").level = logging.INFO
    formatter = logging.Formatter("%(asctime)s %(levelname)s: %(message)s ")
    os.makedirs(app.config["LOG_DIR"], exist_ok=True)
    file_handler = StampedTimedRotatingFileHandler(
        app.config["LOG_PATH"],
        when="D",
        interval=30,
        backupCount=12,
    )
    file_handler.setLevel(app.config["LOG_LEVEL"])
    file_handler.setFormatter(formatter)
    global tim_logger
    tim_logger.handlers = []
    tim_logger.propagate = False
    tim_logger.addHandler(file_handler)
    app.logger.addHandler(file_handler)
    if app.config["LOG_LEVEL_STDOUT"] is not None:
        stdout_handler = logging.StreamHandler(stream=sys.stdout)
        stdout_handler.setLevel(app.config["LOG_LEVEL_STDOUT"])
        stdout_handler.setFormatter(
            logging.Formatter("%(asctime)s %(levelname)s: %(message)s")
        )
        tim_logger.addHandler(stdout_handler)


def enable_loggers() -> None:
    tim_logger.disabled = False
    wz.disabled = False


def log_debug(message: str) -> None:
    tim_logger.debug(message)


def log_info(message: str) -> None:
    tim_logger.info(message)


def log_error(message: str) -> None:
    tim_logger.error(message)


def log_warning(message: str) -> None:
    tim_logger.warning(message)
