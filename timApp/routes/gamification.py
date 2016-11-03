""" Routes for gamification """

from flask import Blueprint, render_template
from .logger import log_info

gamification = Blueprint('gamification', __name__, url_prefix='')


@gamification.route('/gamification')
def is_gamified():
    log_info("Success!")
    return render_template('')
