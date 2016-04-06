"""Routes for settings view."""
import os
from flask import Blueprint, render_template
from jinja2 import TemplateNotFound
from .common import *

settings_page = Blueprint('settings_page',
                          __name__,
                          url_prefix='/settings')


def get_custom_style_files():
    return [file[:-5] for file in os.listdir('static/css') if file.endswith('.scss')]


@settings_page.route('/')
def show():
    verifyLoggedIn()
    css_file_names = get_custom_style_files()
    css_file_descriptions = []
    for file_name in css_file_names:
        default_comment = 'No description.'
        with open(os.path.join('static/css', file_name + '.scss'), 'r', encoding='utf-8') as f:
            comment = f.readline()
            if comment.startswith('@charset'):
                comment = f.readline()
        m = re.match(r'/\* ([^*]+) \*/', comment)
        if m is not None:
            comment = m.groups()[0]
        else:
            comment = default_comment
        css_file_descriptions.append(comment)

    available_css_files = [{'name': name, 'desc': desc} for name, desc in zip(css_file_names, css_file_descriptions)]

    try:
        return render_template('settings.html', css_files=available_css_files)
    except TemplateNotFound:
        abort(404)


@settings_page.route('/save', methods=['POST'])
def save_settings():
    verifyLoggedIn()
    timdb = getTimDb()
    prefs = request.get_json()
    timdb.users.set_preferences(getCurrentUserId(), json.dumps(prefs))
    show()  # Regenerate CSS
    return jsonResponse(get_preferences())
