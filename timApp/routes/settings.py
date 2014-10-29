"""Routes for settings view."""
import os
import cssutils
from flask import Blueprint, render_template, request
from jinja2 import TemplateNotFound
from .common import *

settings_page = Blueprint('settings_page',
                          __name__,
                          url_prefix='/settings')


@settings_page.route('/')
def show():
    verifyLoggedIn()
    timdb = getTimDb()
    prefs = timdb.users.getPrefs(getCurrentUserId())
    prefs = json.loads(prefs) if prefs is not None else {}
    css_file_names = [file for file in os.listdir('static/css') if file.endswith('.css')]
    css_file_descriptions = []
    parser = cssutils.CSSParser()
    for file_name in css_file_names:
        sheet = parser.parseFile(os.path.join('static/css', file_name), 'utf-8')
        comment = 'No description.'
        for rule in sheet.cssRules:
            if isinstance(rule, cssutils.css.CSSComment):
                comment = rule.cssText[2:-2].strip()
                break
        css_file_descriptions.append(comment)
    available_css_files = [{'name': name[:-4], 'desc': desc} for name, desc in zip(css_file_names, css_file_descriptions)]

    try:
        return render_template('settings.html', css_files=available_css_files, settings=prefs)
    except TemplateNotFound:
        abort(404)


@settings_page.route('/save', methods=['POST'])
def save_settings():
    verifyLoggedIn()
    timdb = getTimDb()
    prefs = request.get_json()
    timdb.users.setPrefs(getCurrentUserId(), json.dumps(prefs))
    return "Success"
