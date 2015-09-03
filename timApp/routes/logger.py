"""Routes for logging."""
from .common import *
from flask import Blueprint, request

import logging
import os

logger_bp = Blueprint('logger_bp',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@logger_bp.record
def record_params(setup_state):
    app = setup_state.app

    # current_app.logging.basicConfig(filename='timLog.log',level=logging.DEBUG, format='%(asctime)s %(message)s')
    formatter = logging.Formatter("{\"time\":%(asctime)s, \"file\": %(pathname)s, \"line\" :%(lineno)d, \"messageLevel\":  %(levelname)s, \"message\": %(message)s}")
    if not os.path.exists(app.config['LOG_DIR']):
        try:
            os.mkdir(app.config['LOG_DIR'])
        except FileExistsError:
            pass
    handler = logging.FileHandler(app.config['LOG_PATH'])
    handler.setLevel(logging.DEBUG)
    handler.setFormatter(formatter)
    app.logger.addHandler(handler)

    global logger
    logger = app.logger

    global LOG_LEVELS
    LOG_LEVELS = {"CRITICAL" : app.logger.critical,
                  "ERROR" : app.logger.error,
                  "WARNING" : app.logger.warning,
                  "INFO": app.logger.info,
                  "DEBUG" : app.logger.debug}



# Logger call
@logger_bp.route("/log/", methods=["POST"])
def logMessageRoute():
    jsondata = request.get_json()
    message = jsondata['message']
    level = jsondata['level']
    logMessage(message, level)
    return jsonResponse({}, 200)

def logMessage(message, level):
    try:
        global LOG_LEVELS
        LOG_LEVELS[level](message)
    except KeyError:
        global logger
        logger.error("Failed logging call: " + str(request.get_data()))
