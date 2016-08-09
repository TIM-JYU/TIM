"""Sets up logging for the application."""
import logging
import os

import sys

tim_logger = logging.getLogger('tim')
tim_logger.setLevel(logging.DEBUG)

ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)

tim_logger.addHandler(ch)


def setup_logging(app):
    formatter = logging.Formatter(
        '{"time":%(asctime)s, "file": %(pathname)s, "line" :%(lineno)d, "messageLevel":  %(levelname)s, "message": %(message)s}')
    if not os.path.exists(app.config['LOG_DIR']):
        try:
            os.mkdir(app.config['LOG_DIR'])
        except FileExistsError:
            pass
    file_handler = logging.FileHandler(app.config['LOG_PATH'])
    file_handler.setLevel(app.config['LOG_LEVEL'])
    file_handler.setFormatter(formatter)
    global tim_logger
    tim_logger.handlers = []
    tim_logger.propagate = False
    tim_logger.addHandler(file_handler)
    if app.config['LOG_LEVEL_STDOUT'] is not None:
        stdout_handler = logging.StreamHandler(stream=sys.stdout)
        stdout_handler.setLevel(app.config['LOG_LEVEL_STDOUT'])
        stdout_handler.setFormatter(logging.Formatter('%(levelname)s: %(message)s'))
        tim_logger.addHandler(stdout_handler)


def log_info(message: str):
    tim_logger.info(message)


def log_error(message: str):
    tim_logger.error(message)


def log_warning(message: str):
    tim_logger.warning(message)
