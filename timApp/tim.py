# -*- coding: utf-8 -*-
import time
import traceback
from dataclasses import dataclass
from typing import Optional, List, Any
from urllib.parse import urlparse

import bs4
import requests
from bs4 import BeautifulSoup
from flask import Response, send_file
from flask import g
from flask import redirect
from flask import render_template
from flask import request
from flask import session
from flask_assets import Environment
from flask_wtf.csrf import generate_csrf
from jinja2 import Undefined
from requests.exceptions import MissingSchema, InvalidURL
from sqlalchemy import event
from werkzeug.middleware.profiler import ProfilerMiddleware

from timApp.admin.cli import register_clis
from timApp.admin.global_notification import global_notification
from timApp.admin.routes import admin_bp
from timApp.answer.feedbackanswer import feedback
from timApp.answer.routes import answers
from timApp.auth.accesshelper import verify_edit_access, verify_logged_in
from timApp.auth.login import login_page
from timApp.auth.saml import saml
from timApp.auth.sessioninfo import get_current_user_object, get_other_users_as_list, logged_in
from timApp.backup.backup_routes import backup
from timApp.bookmark.bookmarks import Bookmarks
from timApp.bookmark.routes import bookmarks, add_to_course_bookmark
from timApp.defaultconfig import SECRET_KEY
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
from timApp.item.distribute_rights import dist_bp
from timApp.item.manage import manage_page
from timApp.item.routes import view_page
from timApp.item.routes_tags import tags_blueprint
from timApp.item.tag import Tag, GROUP_TAG_PREFIX
from timApp.lecture.lectureutils import get_current_lecture_info
from timApp.lecture.routes import lecture_routes
from timApp.messaging.messagelist.mailman_events import mailman_events
from timApp.messaging.messagelist.routes import messagelist
from timApp.messaging.timMessage.routes import timMessage
from timApp.modules.fields.cbcountfield import cbcountfield_route
from timApp.note.routes import notes
from timApp.notification.notify import notify
from timApp.plugin.importdata.importData import importData_plugin
from timApp.plugin.qst.qst import qst_plugin
from timApp.plugin.routes import plugin_bp
from timApp.plugin.tableform.tableForm import tableForm_plugin
from timApp.plugin.tape.tape import tape_plugin
from timApp.plugin.timmenu.timMenu import timMenu_plugin
from timApp.plugin.timtable.timTable import timTable_plugin
from timApp.plugin.userselect.userselect import user_select_plugin
from timApp.printing.print import print_blueprint
from timApp.readmark.routes import readings
from timApp.scheduling.scheduling_routes import scheduling
from timApp.sisu.scim import scim
from timApp.sisu.sisu import sisu
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.upload.upload import upload
from timApp.user.groups import groups
from timApp.user.settings.settings import settings_page
from timApp.user.usergroup import UserGroup
from timApp.util.flask.cache import cache
from timApp.util.flask.requesthelper import get_request_message, use_model, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response, add_csp_header
from timApp.util.flask.search import search_routes
from timApp.util.logger import log_info, log_debug
from timApp.util.utils import get_current_time
from timApp.velp.annotation import annotations
from timApp.velp.velp import velps

cache.init_app(app)

blueprints = [
    admin_bp,
    annotations,
    answers,
    backup,
    clipboard,
    course_blueprint,
    dist_bp,
    doc_bp,
    edit_page,
    feedback,
    generateMap,
    global_notification,
    groups,
    saml,
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
    tags_blueprint,
    tr_bp,
    upload,
    velps,
    view_page,
    scheduling,
    mailman_events,

    # plugins
    importData_plugin,
    qst_plugin,
    tableForm_plugin,
    tape_plugin,
    cbcountfield_route,
    timMenu_plugin,
    timTable_plugin,
    user_select_plugin,
    messagelist,
    timMessage,
]

if app.config['BOOKMARKS_ENABLED']:
    app.register_blueprint(bookmarks)

for bp in blueprints:
    app.register_blueprint(bp)

assets = Environment(app)

register_errorhandlers(app)
register_clis(app)


def shim_jinja_undefined_contains(cls: Any, k: Any) -> bool:
    return False


# This is a temporary fix because of this change in Jinja 3.0.0:
# https://github.com/pallets/jinja/pull/1204
#
# The change caused `a in b` checks fail for undefined fields in jinja templates
# This shim emulates an official fix that will be merged into Jinja 3.0.2:
# https://github.com/pallets/jinja/pull/1455
# TODO: Remove once Jinja is updated to 3.0.2
Undefined.__contains__ = classmethod(shim_jinja_undefined_contains)


@app.context_processor
def inject_custom_css() -> dict:
    """Injects the user prefs variable to all templates."""
    prefs = get_current_user_object().get_prefs()
    return dict(prefs=prefs)


@app.context_processor
def inject_angular_scripts() -> dict:
    """Provides the JavaScript files compiled by Angular."""
    locale = get_locale()
    try:
        # Angular compiler modifies the base href of index.html to redirect the scripts to correct location,
        # but it does not work for TIM because the HTML is dynamically generated and modifying base href would break
        # other links. So we modify the script by hand.
        # TODO: Cache the modified result.
        return get_angularscripts(f'static/scripts/build/{locale}/index.html', locale=locale)
    except FileNotFoundError:
        try:
            return get_angularscripts(f'static/scripts/build/index.html')
        except FileNotFoundError:
            raise Exception(
                'TypeScript files have not been built (compiled JavaScript files are missing).\n'
                'If this is a local development TIM instance, start the "bdw" NPM script (in timApp/package.json) '
                'from your IDE.\n'
                'If this is not a local TIM instance, run "./js" from TIM root.'
            )


def get_angularscripts(index_file: str, locale: Optional[str] = None):
    with open(index_file) as f:
        html_data = f.read()
        bs = BeautifulSoup(html_data, 'lxml')
        scripts: List[bs4.element.Tag] = [e for e in bs.find_all('script')]
        n = BeautifulSoup("", 'lxml')
        style = bs.find('link')
        for s in scripts:
            n.append(s)
            if locale:
                s['src'] += f'?l={locale}'  # The parameter is only needed for cache busting (for Chrome).

        # Only production config has extractCss enabled, so this will be None for a non-prod build.
        # TODO: this is possibly always True after upgrading to Angular 11.
        if style:
            n.append(style)
        return dict(angularscripts=str(n))


KNOWN_LANGUAGES = [
    'fi',
    'en-US',
]


def get_locale():
    header_lang = request.accept_languages.best_match(KNOWN_LANGUAGES, default='en-US')
    lng = request.cookies.get('lang')
    if not lng:
        if not logged_in():
            return header_lang
        u = get_current_user_object()
        lng = u.get_prefs().language
    if lng in KNOWN_LANGUAGES:
        return lng
    return header_lang


@app.context_processor
def inject_user() -> dict:
    """"Injects user-related info to all templates."""
    r = dict(
        current_user=get_current_user_object(),
        lecture_info=get_current_lecture_info(),
        other_users=get_other_users_as_list(),
        locale=get_locale(),
    )
    if logged_in() and app.config['BOOKMARKS_ENABLED']:
        r['bookmarks'] = get_current_user_object().bookmarks.as_dict()
    return r


@app.get('/js/<path:path>')
def get_js_file(path: str):
    locale = get_locale()
    for f in [
        f'static/scripts/build/{locale}/{path}',
        f'static/scripts/build/{path}',
    ]:
        try:
            return send_file(f)
        except FileNotFoundError:
            pass
    raise NotExist('File not found')


@app.get('/empty')
def empty_response_route():
    return Response('', mimetype='text/plain')


@app.get("/ping")
def ping():
    return ok_response()


@dataclass
class GetProxyModel:
    url: str
    auth_token: Optional[str] = None
    raw: bool = False
    mimetype: Optional[str] = None


@app.get("/getproxy")
@use_model(GetProxyModel)
def getproxy(m: GetProxyModel):
    parsed = urlparse(m.url)
    if not parsed.scheme:
        raise RouteException('Unknown URL scheme')
    if parsed.scheme not in ('http', 'https'):
        raise RouteException(f'URL scheme not allowed: {parsed.scheme}')
    if parsed.netloc not in app.config['PROXY_WHITELIST']:
        raise RouteException(f'URL domain not whitelisted: {parsed.netloc}')
    if parsed.netloc not in app.config['PROXY_WHITELIST_NO_LOGIN']:
        verify_logged_in()
    headers = {}
    if m.auth_token:
        headers['Authorization'] = f'Token {m.auth_token}'
    try:
        r = requests.get(m.url, headers=headers)
    except (MissingSchema, InvalidURL):
        raise RouteException('Invalid URL')
    if m.raw:
        mimetype = r.headers['Content-Type']
        if m.mimetype:
            mimetype = m.mimetype
        resp = Response(
            r.content,
            status=r.status_code,
            mimetype=mimetype,
        )
        add_csp_header(resp, 'sandbox allow-scripts')
        return resp

    return json_response({'data': r.text, 'status_code': r.status_code})


@app.get("/time")
def get_time():
    return json_response({'time': get_current_time()}, date_conversion=True)


@app.get("/getTemplates")
def get_templates():
    current_path = request.args.get('item_path', '')
    d = DocEntry.find_by_path(current_path)
    if not d:
        raise NotExist()
    verify_edit_access(d)
    templates = get_templates_for_folder(d.parent)
    return json_response(templates, date_conversion=True)


def update_user_course_bookmarks():
    u = get_current_user_object()
    for gr in u.groups:  # type: UserGroup
        if gr.is_sisu_student_group:
            docs = DocEntry.query.join(Block).join(Tag).filter(Tag.name == GROUP_TAG_PREFIX + gr.name).with_entities(
                DocEntry).all()
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


@app.get("/en")
@app.get("/fi")
@app.get("/")
def start_page():
    update_user_course_bookmarks()
    db.session.commit()
    return render_template(
        'start.jinja2',
    )


def install_sql_hook():
    prev_exec_time = get_current_time()

    @event.listens_for(db.engine, 'before_execute')
    def receive_before_execute(conn, clauseelement, multiparams, params):
        nonlocal prev_exec_time
        curr = get_current_time()
        print(f'--------------------------------------TIMING: {curr} ({curr - prev_exec_time})')
        prev_exec_time = curr
        for r in traceback.format_stack():
            if r.startswith('  File "/service/') and not receive_before_execute.__name__ in r:
                print(r, end='')
        print(clauseelement)


if app.config['DEBUG_SQL']:
    install_sql_hook()


@app.before_request
def preprocess_request():
    session.permanent = True
    g.request_start_time = time.monotonic()
    if request.method == 'GET':
        p = request.path
        if '//' in p or (p.endswith('/') and p != '/'):
            fixed_url = p.rstrip('/').replace('//', '/') + '?' + request.query_string.decode()
            return redirect(fixed_url)


def should_log_request():
    p = request.path
    if p.startswith('/static/'):
        raise Exception('static files should be served by Caddy, not Flask')
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
def after_request(resp: Response):
    token = generate_csrf()
    resp.set_cookie(
        'XSRF-TOKEN',
        token,
        samesite=app.config['SESSION_COOKIE_SAMESITE'],
        secure=app.config['SESSION_COOKIE_SECURE'],
    )
    if not request.cookies.get('lang'):
        resp.set_cookie('lang', get_locale())
    return resp


@app.teardown_appcontext
def close_db_appcontext(_e):
    if not app.config['TESTING'] and hasattr(g, 'timdb'):
        g.timdb.close()


def init_app():
    if app.config['PROFILE']:
        app.wsgi_app = ProfilerMiddleware(app.wsgi_app, sort_by=('cumtime',), restrictions=[100])

    for var in [
        'DB_URI',
        'DEBUG',
        'MAIL_HOST',
        'PG_MAX_CONNECTIONS',
        'PLUGIN_CONNECT_TIMEOUT',
        'PROFILE',
        'SQLALCHEMY_MAX_OVERFLOW',
        'SQLALCHEMY_POOL_SIZE',
    ]:
        log_info(f'{var}: {app.config.get(var, "(undefined)")}')
    if not app.config['DEBUG']:
        if app.config['SECRET_KEY'] == SECRET_KEY:
            raise Exception('SECRET_KEY must not be the same as default SECRET_KEY when DEBUG=False')
    return app


def start_app() -> None:
    init_app()
    app.run(host='0.0.0.0',
            port=5000,
            use_evalex=False,
            use_reloader=False,
            threaded=True)
