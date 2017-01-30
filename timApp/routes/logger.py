"""Sets up logging for the application."""
import logging
import os

import sys
from logging.handlers import RotatingFileHandler

tim_logger = logging.getLogger('tim')
tim_logger.setLevel(logging.DEBUG)

wz = logging.getLogger('werkzeug')
wz.setLevel(logging.ERROR)

ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)

tim_logger.addHandler(ch)


def setup_logging(app):
    if not app.config['TESTING']:
        logging.getLogger('alembic').level = logging.INFO
    formatter = logging.Formatter('%(asctime)s %(levelname)s: %(message)s ')
    if not os.path.exists(app.config['LOG_DIR']):
        try:
            os.mkdir(app.config['LOG_DIR'])
        except FileExistsError:
            pass
    file_handler = RotatingFileHandler(app.config['LOG_PATH'], maxBytes=1024 * 1024 * 100, backupCount=1000)
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


def enable_loggers():
    tim_logger.disabled = False
    wz.disabled = False


def log_debug(message: str):
    tim_logger.debug(message)


def log_info(message: str):
    tim_logger.info(message)


def log_error(message: str):
    tim_logger.error(message)


def log_warning(message: str):
    tim_logger.warning(message)
