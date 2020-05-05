"""Sets up logging for the application."""
import logging
import os

import sys

from flask import Flask

tim_logger = logging.getLogger('tim')
tim_logger.setLevel(logging.DEBUG)

wz = logging.getLogger('werkzeug')
wz.setLevel(logging.ERROR)


def setup_logging(app: Flask) -> None:
    if not app.config['TESTING']:
        logging.getLogger('alembic').level = logging.INFO
    formatter = logging.Formatter('%(asctime)s %(levelname)s: %(message)s ')
    os.makedirs(app.config['LOG_DIR'], exist_ok=True)
    file_handler = logging.FileHandler(app.config['LOG_PATH'])
    file_handler.setLevel(app.config['LOG_LEVEL'])
    file_handler.setFormatter(formatter)
    global tim_logger
    tim_logger.handlers = []
    tim_logger.propagate = False
    tim_logger.addHandler(file_handler)
    app.logger.addHandler(file_handler)
    if app.config['LOG_LEVEL_STDOUT'] is not None:
        stdout_handler = logging.StreamHandler(stream=sys.stdout)
        stdout_handler.setLevel(app.config['LOG_LEVEL_STDOUT'])
        stdout_handler.setFormatter(logging.Formatter('%(levelname)s: %(message)s'))
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
