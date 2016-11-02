""" Routes for gamification """
from .common import *
from flask import Blueprint
import json
from .logger import log_info


gamification = Blueprint('gamification', __name__, url_prefix='')


@gamification.route('/gamification')


def gamification():
    return log_info ("Success!")
