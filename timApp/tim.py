# -*- coding: utf-8 -*-

import time
from urllib.parse import urlparse

import requests
from dataclasses import dataclass
from flask import Response
from flask import g, abort
from flask import redirect
from flask import render_template
from flask import request
from flask import session
from flask_assets import Environment
from flask_wtf.csrf import generate_csrf
from requests.exceptions import MissingSchema, InvalidURL
from werkzeug.middleware.profiler import ProfilerMiddleware

from timApp.admin.global_notification import global_notification
from timApp.admin.routes import admin_bp
from timApp.answer.feedbackanswer import feedback
from timApp.answer.routes import answers
from timApp.auth.accesshelper import verify_edit_access, verify_logged_in
from timApp.auth.login import login_page
from timApp.auth.sessioninfo import get_current_user_object, get_other_users_as_list, get_current_user_id, \
    logged_in, current_user_in_lecture
from timApp.bookmark.bookmarks import Bookmarks
from timApp.bookmark.routes import bookmarks, add_to_course_bookmark
from timApp.document.course.routes import course_blueprint
from timApp.document.course.validate import is_course
from timApp.document.create_item import get_templates_for_folder
from timApp.document.docentry import DocEntry
from timApp.document.editing.routes import edit_page
from timApp.document.editing.routes_clipboard import clipboard
from timApp.document.minutes.routes import minutes_blueprint
from timApp.document.routes import doc_bp
from timApp.document.translation.routes import tr_bp
from timApp.errorhandlers import register_errorhandlers
from timApp.gamification.generateMap import generateMap
from timApp.item.block import Block
from timApp.item.manage import manage_page
from timApp.item.routes import view_page
from timApp.item.routes_tags import tags_blueprint
from timApp.item.tag import Tag, GROUP_TAG_PREFIX
from timApp.lecture.routes import lecture_routes
from timApp.note.routes import notes
from timApp.notification.notify import notify
from timApp.plugin.importdata.importData import importData_plugin
from timApp.plugin.qst.qst import qst_plugin
from timApp.plugin.routes import plugin_bp
from timApp.plugin.tableform.tableForm import tableForm_plugin
from timApp.plugin.tape.tape import tape_plugin
from timApp.plugin.timmenu.timMenu import timMenu_plugin
from timApp.plugin.timtable.timTable import timTable_plugin
from timApp.printing.print import print_blueprint
from timApp.readmark.routes import readings
from timApp.sisu.scim import scim
from timApp.sisu.sisu import sisu
from timApp.tim_app import app, default_secret
from timApp.upload.upload import upload
from timApp.user.groups import groups
from timApp.user.settings.settings import settings_page
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.ReverseProxied import ReverseProxied
from timApp.util.flask.cache import cache
from timApp.util.flask.requesthelper import get_request_message, use_model, RouteException
from timApp.util.flask.responsehelper import json_response, ok_response, text_response
from timApp.util.flask.routes_static import static_bp
from timApp.util.flask.search import search_routes
from timApp.util.logger import log_info, log_debug, log_warning
from timApp.velp.annotation import annotations
from timApp.velp.velp import velps

cache.init_app(app)

for bp in [
    admin_bp,
    annotations,
    answers,
    bookmarks,
    clipboard,
    course_blueprint,
    doc_bp,
    edit_page,
    feedback,
    generateMap,
    global_notification,
    groups,
    lecture_routes,
    login_page,
    manage_page,
    minutes_blueprint,
    notes,
    notify,
    plugin_bp,
    print_blueprint,
    readings,
    scim,
    search_routes,
    settings_page,
    sisu,
    static_bp,
    tags_blueprint,
    tr_bp,
    upload,
    velps,
    view_page,

    # plugins
    importData_plugin,
    qst_plugin,
    tableForm_plugin,
    tape_plugin,
    timMenu_plugin,
    timTable_plugin,
]:
    app.register_blueprint(bp)


app.wsgi_app = ReverseProxied(app.wsgi_app)

assets = Environment(app)

register_errorhandlers(app)

@app.context_processor
def inject_custom_css() -> dict:
    """Injects the user prefs variable to all templates."""
    prefs = get_current_user_object().get_prefs()
    return dict(prefs=prefs)


@app.context_processor
def inject_user() -> dict:
    """"Injects the user object to all templates."""
    return dict(
        current_user=get_current_user_object(),
        other_users=get_other_users_as_list(),
    )


@app.context_processor
def inject_bookmarks() -> dict:
    """"Injects bookmarks to all templates."""
    if not logged_in():
        return {}
    return dict(bookmarks=Bookmarks(User.query.get(get_current_user_id())).as_dict())


@app.route('/empty')
def empty_response_route():
    return Response('', mimetype='text/plain')


@app.route("/ping")
def ping():
    return ok_response()


@dataclass
class GetProxyModel:
    url: str


@app.route("/getproxy")
@use_model(GetProxyModel)
def getproxy(m: GetProxyModel):
    verify_logged_in()
    parsed = urlparse(m.url)
    if not parsed.scheme:
        raise RouteException('Unknown URL scheme')
    if parsed.scheme not in ('http', 'https'):
        raise RouteException(f'URL scheme not allowed: {parsed.scheme}')
    if parsed.netloc not in app.config['PROXY_WHITELIST']:
        raise RouteException(f'URL domain not whitelisted: {parsed.netloc}')
    try:
        r = requests.get(m.url)
    except (MissingSchema, InvalidURL):
        raise RouteException('Invalid URL')

    text = r.content
    return text_response(text, r.status_code)


@app.route("/getTemplates")
def get_templates():
    current_path = request.args.get('item_path', '')
    d = DocEntry.find_by_path(current_path)
    if not d:
        abort(404)
    verify_edit_access(d)
    templates = get_templates_for_folder(d.parent)
    return json_response(templates)


def update_user_course_bookmarks():
    u = get_current_user_object()
    for gr in u.groups:  # type: UserGroup
        if gr.is_sisu_student_group:
            docs = DocEntry.query.join(Block).join(Tag).filter(Tag.name == GROUP_TAG_PREFIX + gr.name).with_entities(DocEntry).all()
            if not docs:
                continue
            if len(docs) > 1:
                continue
            d: DocEntry = docs[0]
            if not is_course(d):
                continue
            if d.document.get_settings().sisu_require_manual_enroll():
                continue
            b = Bookmarks(u)
            add_to_course_bookmark(b, d)


@app.route("/en")
@app.route("/fi")
@app.route("/")
def start_page():
    in_lecture = current_user_in_lecture()
    update_user_course_bookmarks()
    return render_template('start.html',
                           in_lecture=in_lecture)


@app.before_request
def preprocess_request():
    session.permanent = True
    g.request_start_time = time.monotonic()
    if request.method == 'GET':
        p = request.path
        if '//' in p or (p.endswith('/') and p != '/'):
            fixed_url = p.rstrip('/').replace('//', '/') + '?' + request.query_string.decode()
            return redirect(fixed_url)


@app.after_request
def disable_cache_for_testing(response):
    """Chrome WebDriver caches JavaScript files which causes SystemJS.import('tim') to not get executed properly.
    It is either WebDriver or SystemJS bug.
    
    The workaround is to disable the cache.
    """
    if app.config['TESTING']:
        response.headers["Cache-Control"] = "max-age=0, must-revalidate"
    return response


def should_log_request():
    p = request.path
    # don't log OpenID completion URL because it contains email etc.
    if p.startswith('/openIDLogin') and b'&openid_complete=yes' in request.query_string:
        return False
    if p.startswith('/static/'):
        return False
    if p == '/favicon.ico':
        return False
    return True


@app.after_request
def log_request(response):
    if should_log_request():
        status_code = response.status_code
        log_info(get_request_message(status_code))
        if request.method in ('PUT', 'POST', 'DELETE'):
            log_debug(request.get_json(silent=True))
    return response


@app.after_request
def close_db(response):
    if hasattr(g, 'timdb'):
        g.timdb.close()
    return response


@app.after_request
def del_g(response):
    """For some reason, the g object is not cleared when running browser test, so we do it here."""
    if app.config['TESTING']:
        if hasattr(g, 'user'):
            del g.user
        if hasattr(g, 'viewable'):
            del g.viewable
        if hasattr(g, 'editable'):
            del g.editable
        if hasattr(g, 'teachable'):
            del g.teachable
        if hasattr(g, 'manageable'):
            del g.manageable
        if hasattr(g, 'see_answers'):
            del g.see_answers
        if hasattr(g, 'owned'):
            del g.owned
    return response


@app.after_request
def after_request(resp):
    token = generate_csrf()
    resp.set_cookie('XSRF-TOKEN', token)
    return resp


# noinspection PyUnusedLocal
@app.teardown_appcontext
def close_db_appcontext(e):
    if not app.config['TESTING'] and hasattr(g, 'timdb'):
        g.timdb.close()


def init_app():
    with app.app_context():
        cache.clear()

    if app.config['PROFILE']:
        app.wsgi_app = ProfilerMiddleware(app.wsgi_app, sort_by=('cumtime',), restrictions=[100])

    log_info(f'Debug mode: {app.config["DEBUG"]}')
    log_info(f'Profiling: {app.config["PROFILE"]}')
    log_info(f'Using database: {app.config["DATABASE"]}')
    if not app.config.from_pyfile(app.config['SECRET_FILE_PATH'], silent=True):
        log_warning('secret file not found, using default values - do not run in production!')
    else:
        assert default_secret != app.config['SECRET_KEY']
    return app


def start_app():
    init_app()
    app.run(host='0.0.0.0',
            port=5000,
            use_evalex=False,
            use_reloader=False,
            threaded=True)
