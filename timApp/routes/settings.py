"""Routes for settings view."""
from flask import Blueprint, render_template
from flask import abort
from flask import request
from jinja2 import TemplateNotFound

from accesshelper import verify_logged_in
from responsehelper import json_response
from sessioninfo import get_current_user_object
from theme import get_available_themes

settings_page = Blueprint('settings_page',
                          __name__,
                          url_prefix='/settings')


@settings_page.before_request
def verify_login():
    verify_logged_in()


@settings_page.route('/')
def show():
    available_css_files = [{'name': theme.filename, 'desc': theme.description} for theme in get_available_themes()]

    try:
        return render_template('settings.html', css_files=available_css_files)
    except TemplateNotFound:
        abort(404)


@settings_page.route('/save', methods=['POST'])
def save_settings():
    get_current_user_object().set_prefs(request.get_json())
    show()  # Regenerate CSS
    return json_response(get_current_user_object().get_prefs())


@settings_page.route('/get/<name>')
def get_setting(name):
    return json_response({name: get_current_user_object().get_prefs().get(name)})
