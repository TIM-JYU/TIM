"""Routes for settings view."""
from flask import Blueprint, render_template
from jinja2 import TemplateNotFound

from routes.accesshelper import verify_logged_in
from theme import get_available_themes
from .common import *

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
    update_preferences(request.get_json())
    show()  # Regenerate CSS
    return jsonResponse(get_preferences())


@settings_page.route('/get/<name>')
def get_setting(name):
    return jsonResponse({name: get_preferences().get(name)})
