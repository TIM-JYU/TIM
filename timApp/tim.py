import re
import time
import traceback
from urllib.parse import urlparse

import bs4
from bs4 import BeautifulSoup
from flask import Response
from flask import g
from flask import redirect
from flask import render_template
from flask import request
from flask import session
from flask_assets import Environment
from flask_wtf.csrf import generate_csrf
from sqlalchemy import event
from werkzeug.middleware.profiler import ProfilerMiddleware

from timApp.admin.cli import register_clis
from timApp.admin.global_notification import global_notification
from timApp.admin.routes import admin_bp
from timApp.answer.feedbackanswer import feedback
from timApp.answer.routes import answers
from timApp.auth.access.routes import access
from timApp.auth.login import login_page
from timApp.auth.logincodes.routes import login_codes
from timApp.auth.oauth2.oauth2 import init_oauth
from timApp.auth.saml.routes import saml
from timApp.auth.session.routes import user_sessions
from timApp.auth.sessioninfo import (
    get_current_user_object,
    get_other_users_as_list,
    logged_in,
)
from timApp.backup.backup_routes import backup
from timApp.bookmark.course import update_user_course_bookmarks
from timApp.bookmark.routes import bookmarks
from timApp.defaultconfig import SECRET_KEY
from timApp.document.course.routes import course_blueprint
from timApp.document.editing.routes import edit_page
from timApp.document.editing.routes_clipboard import clipboard
from timApp.document.minutes.routes import minutes_blueprint
from timApp.document.routes import doc_bp
from timApp.document.translation.routes import tr_bp
from timApp.gamification.generateMap import generateMap
from timApp.item.distribute_rights import dist_bp
from timApp.item.manage import manage_page
from timApp.item.routes import view_page
from timApp.item.routes_tags import tags_blueprint
from timApp.lecture.lectureutils import get_current_lecture_info
from timApp.lecture.routes import lecture_routes
from timApp.messaging.messagelist.emaillist import check_mailman_connection
from timApp.messaging.messagelist.mailman_events import (
    mailman_events,
    has_valid_event_auth,
)
from timApp.messaging.messagelist.routes import messagelist
from timApp.messaging.timMessage.routes import tim_message
from timApp.modules.fields.cbcountfield import cbcountfield_route
from timApp.note.routes import notes
from timApp.notification.notify import notify
from timApp.plugin.calendar.calendar import calendar_plugin
from timApp.plugin.examGroupManager.examGroupManager import exam_group_manager_plugin
from timApp.plugin.group_join.group_join import group_join_plugin
from timApp.plugin.importdata.importData import importData_plugin
from timApp.plugin.qst.qst import qst_plugin
from timApp.plugin.quantum_circuit.quantumCircuit import quantum_circuit_plugin
from timApp.plugin.reviewcanvas.reviewcanvas import reviewcanvas_plugin
from timApp.plugin.routes import plugin_bp
from timApp.plugin.steps.steps import steps_plugin
from timApp.plugin.symbolbutton.symbolbutton import symbolbutton_plugin
from timApp.plugin.tableform.tableForm import tableForm_plugin
from timApp.plugin.tape.tape import tape_plugin
from timApp.plugin.timmenu.timMenu import timMenu_plugin
from timApp.plugin.timtable.timTable import timTable_plugin
from timApp.plugin.userselect.userselect import user_select_plugin
from timApp.printing.print import print_blueprint
from timApp.proxy.routes import proxy
from timApp.redirect.routes import redirect_route
from timApp.readmark.routes import readings
from timApp.scheduling.scheduling_routes import scheduling
from timApp.securitytxt.routes import securitytxt
from timApp.sisu.scim import scim
from timApp.sisu.sisu import sisu
from timApp.idesupport.routes import ide
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.upload.upload import upload
from timApp.user.contacts import contacts
from timApp.user.groups import groups
from timApp.user.settings.settings import settings_page
from timApp.user.settings.styles import styles
from timApp.user.verification.routes import verify
from timApp.user_profile.routes import profile_blueprint
from timApp.events.routes import events_blueprint
from timApp.util.error_handlers import register_errorhandlers
from timApp.util.flask.cache import cache
from timApp.util.flask.requesthelper import (
    get_request_message,
    is_testing,
)
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.search import search_routes
from timApp.util.locale import get_locale
from timApp.util.logger import log_info, log_debug
from timApp.util.testing import register_testing_routes
from timApp.util.utils import get_current_time
from timApp.velp.annotation import annotations
from timApp.velp.velp import velps
from flask_socketio import SocketIO, emit, send

cache.init_app(app)

blueprints = [
    access,
    admin_bp,
    annotations,
    answers,
    backup,
    clipboard,
    contacts,
    course_blueprint,
    dist_bp,
    doc_bp,
    edit_page,
    feedback,
    generateMap,
    global_notification,
    group_join_plugin,
    groups,
    saml,
    lecture_routes,
    login_codes,
    login_page,
    manage_page,
    minutes_blueprint,
    notes,
    notify,
    plugin_bp,
    print_blueprint,
    proxy,
    redirect_route,
    readings,
    scim,
    securitytxt,
    search_routes,
    settings_page,
    sisu,
    steps_plugin,
    tags_blueprint,
    styles,
    tr_bp,
    upload,
    velps,
    view_page,
    scheduling,
    mailman_events,
    user_sessions,
    # plugins
    calendar_plugin,
    exam_group_manager_plugin,
    importData_plugin,
    qst_plugin,
    reviewcanvas_plugin,
    tableForm_plugin,
    tape_plugin,
    cbcountfield_route,
    timMenu_plugin,
    timTable_plugin,
    user_select_plugin,
    messagelist,
    tim_message,
    verify,
    quantum_circuit_plugin,
    symbolbutton_plugin,
    ide,
    profile_blueprint,
    events_blueprint,
]

if app.config["BOOKMARKS_ENABLED"]:
    app.register_blueprint(bookmarks)

for bp in blueprints:
    app.register_blueprint(bp)

init_oauth(app)

assets = Environment(app)

register_errorhandlers(app)
register_clis(app)


@app.context_processor
def inject_angular_scripts() -> dict:
    """Provides the JavaScript files compiled by Angular."""
    locale = get_locale()
    try:
        # Angular compiler modifies the base href of index.html to redirect the scripts to correct location,
        # but it does not work for TIM because the HTML is dynamically generated and modifying base href would break
        # other links. So we modify the script by hand.
        # TODO: Cache the modified result.
        return get_angularscripts(
            f"static/scripts/build/{locale}/index.html", locale=locale
        )
    except FileNotFoundError:
        try:
            return get_angularscripts(f"static/scripts/build/index.html")
        except FileNotFoundError:
            # Don't raise issues for missing JS during testing. JS might not be built to speed up tests.
            if is_testing():
                return dict(angularscripts="")
            raise Exception(
                "TypeScript files have not been built (compiled JavaScript files are missing).\n"
                'If this is a local development TIM instance, start the "bdw" NPM script (in timApp/package.json) '
                "from your IDE.\n"
                'If this is not a local TIM instance, run "./tim js" from TIM root.'
            )


def get_angularscripts(index_file: str, locale: str | None = None):
    with open(index_file) as f:
        html_data = f.read()
        bs = BeautifulSoup(html_data, "lxml")
        scripts: list[bs4.element.Tag] = [e for e in bs.find_all("script")]
        n = BeautifulSoup("", "lxml")
        style = bs.find("link")
        for s in scripts:
            n.append(s)
            if locale:
                s[
                    "src"
                ] += f"?l={locale}"  # The parameter is only needed for cache busting (for Chrome).

        # Only production config has extractCss enabled, so this will be None for a non-prod build.
        # TODO: this is possibly always True after upgrading to Angular 11.
        if style:
            n.append(style)
        return dict(angularscripts=str(n))


@app.context_processor
def inject_user() -> dict:
    """ "Injects user-related info to all templates."""
    r = dict(
        current_user=get_current_user_object(),
        lecture_info=get_current_lecture_info(),
        other_users=get_other_users_as_list(),
        locale=get_locale(),
        prefs=get_current_user_object().get_prefs(),
    )
    if logged_in() and app.config["BOOKMARKS_ENABLED"]:
        r["bookmarks"] = get_current_user_object().bookmarks.as_dict()
    return r


@app.get("/js/<path:path>")
def get_js_file(path: str):
    raise Exception("Angular scripts should be served by Caddy, not Flask")


@app.get("/empty")
def empty_response_route():
    return Response("", mimetype="text/plain")


@app.get("/ping")
def ping():
    return ok_response()


@app.get("/time")
def get_time():
    return json_response({"time": get_current_time()}, date_conversion=True)


@app.get("/en")
@app.get("/fi")
@app.get("/sv")
@app.get("/")
def start_page():
    update_user_course_bookmarks()
    db.session.commit()
    return render_template(
        "start.jinja2",
    )


def install_sql_hook():
    prev_exec_time = get_current_time()

    with app.app_context():

        @event.listens_for(db.engine, "before_execute")
        def receive_before_execute(conn, clauseelement, multiparams, params):
            nonlocal prev_exec_time
            curr = get_current_time()
            print(
                f"--------------------------------------TIMING: {curr} ({curr - prev_exec_time})"
            )
            prev_exec_time = curr
            for r in traceback.format_stack():
                if (
                    r.startswith('  File "/service/')
                    and not receive_before_execute.__name__ in r
                ):
                    print(r, end="")
            try:
                print(clauseelement, multiparams, params)
            except Exception as e:
                print(f"<unprintable clauseelement>: {e}")


if app.config["TESTING"]:
    register_testing_routes(app)


if app.config["DEBUG_SQL"]:
    install_sql_hook()

LOG_BEFORE_REQUESTS = app.config["LOG_BEFORE_REQUESTS"]


DOUBLE_SLASH_PATH_REGEX = re.compile(r"/\s*/")


@app.before_request
def preprocess_request():
    session.permanent = True
    g.request_start_time = time.monotonic()
    # Log the request before it is processed.
    if LOG_BEFORE_REQUESTS:
        log_info(get_request_message(include_time=False, is_before=True))

    # Redirect to a canonical URL if the path contains double slashes or ends with a slash.
    if request.method == "GET":
        p = request.path
        if "//" in p or (p.endswith("/") and p != "/"):
            query_str = request.query_string.decode()
            fixed_url = p.rstrip("/")
            fixed_url = DOUBLE_SLASH_PATH_REGEX.sub("/", fixed_url)
            if query_str:
                fixed_url = f"{fixed_url}?{query_str}"
            return redirect(fixed_url)

        # Also, capture the referrer to the session if it is different from the current URL domain.
        if request.referrer:
            # Check if referrer is different from the request domain.
            try:
                referrer_domain = urlparse(request.referrer).netloc
            except ValueError:
                referrer_domain = None
            last_referrers = session.get("last_referrers", [])
            if (
                referrer_domain
                and referrer_domain != request.host
                and referrer_domain not in last_referrers
            ):
                last_referrers.append(referrer_domain)
                # Only leave the last 5 referrers.
                session["last_referrers"] = last_referrers[-3:]


def should_log_request():
    p = request.path
    if p.startswith("/static/"):
        raise Exception("static files should be served by Caddy, not Flask")
    if p == "/favicon.ico":
        return False
    return True


@app.after_request
def log_request(response):
    if should_log_request():
        status_code = response.status_code
        log_info(get_request_message(status_code))
        if request.method in ("PUT", "POST", "DELETE"):
            log_debug(str(request.get_json(silent=True)))
    return response


@app.after_request
def robots_request(response: Response):
    if app.config["RESTRICT_ROBOTS"] and request.method == "GET":
        if app.config["RESTRICT_ROBOTS_METHODS"].get("restrict_global"):
            response.headers.add_header(
                "X-Robots-Tag",
                ", ".join(app.config["RESTRICT_ROBOTS_METHODS"].get("global")),
            )
        else:
            for bot in app.config["RESTRICT_ROBOTS_METHODS"]["bots"].keys():
                restricted_methods = f"{', '.join(app.config['RESTRICT_ROBOTS_METHODS']['bots'].get(bot))}"
                if restricted_methods:
                    value = f"{bot}: {restricted_methods}"
                    response.headers.add_header(
                        "X-Robots-Tag",
                        value,
                    )

        response.headers.add_header(
            "X-Robots-Tag",
            f"unavailable_after: {app.config['RESTRICT_ROBOTS_METHODS'].get('global_unavailable_date')}",
        )
    return response


@app.after_request
def after_request(resp: Response):
    token = generate_csrf()
    resp.set_cookie(
        "XSRF-TOKEN",
        token,
        samesite=app.config["SESSION_COOKIE_SAMESITE"],
        secure=app.config["SESSION_COOKIE_SECURE"],
    )

    locale = get_locale()
    # lang is used to preserve the active language preference which may be either
    # a specific language or "UseWebBrowser" which defaults to browser preference
    if not request.cookies.get("lang"):
        resp.set_cookie(
            "lang",
            locale,
            samesite=app.config["SESSION_COOKIE_SAMESITE"],
            secure=app.config["SESSION_COOKIE_SECURE"],
        )
    # script_lang is used by Caddy to serve correct Angular scripts
    # It always contains a specific valid language, never "UseWebBrowser"
    resp.set_cookie(
        "script_lang",
        locale,
        samesite=app.config["SESSION_COOKIE_SAMESITE"],
        secure=app.config["SESSION_COOKIE_SECURE"],
    )
    return resp


tim_socketio = SocketIO()  # (logger=True, engineio_logger=True)


@tim_socketio.on("connect")
def handle_connect():
    print("Client connected to /chat")
    current_user = get_current_user_object()
    emit(
        "message",
        {
            "type": "message",
            "data": f"Welcome to the WebSocket server {current_user.real_name}!",
        },
        broadcast=True,
    )


@tim_socketio.on("message")
def handle_message(msg):
    print(f"Received message: {msg}")
    emit(
        "message",
        msg,
        broadcast=True,
    )
    # send(msg)


@tim_socketio.on("disconnect")
def handle_disconnect():
    print("Client disconnected from /chat")


def init_app():
    if app.config["PROFILE"]:
        app.wsgi_app = ProfilerMiddleware(
            app.wsgi_app,
            sort_by=("cumtime",),
            restrictions=[100],
            profile_dir="/service/timApp/static/profiling",
        )

    for var in [
        "DB_URI",
        "DEBUG",
        "MAIL_HOST",
        "PG_MAX_CONNECTIONS",
        "PLUGIN_CONNECT_TIMEOUT",
        "PROFILE",
        "SQLALCHEMY_MAX_OVERFLOW",
        "SQLALCHEMY_POOL_SIZE",
    ]:
        log_info(f'{var}: {app.config.get(var, "(undefined)")}')
    if not app.config["DEBUG"]:
        if app.config["SECRET_KEY"] == SECRET_KEY:
            raise Exception(
                "SECRET_KEY must not be the same as default SECRET_KEY when DEBUG=False"
            )

    if app.config["MESSAGE_LISTS_ENABLED"]:
        log_info(f"Mailman client credentials configured: {check_mailman_connection()}")
        log_info(f"Mailman events REST auth configured: {has_valid_event_auth()}")

    tim_socketio.init_app(
        app,
        cors_allowed_origins="*",
        async_mode="gevent",
    )
    return app


def start_app() -> None:
    init_app()

    # app.run(
    #    host="0.0.0.0", port=5000, use_evalex=False, use_reloader=False, threaded=True
    # )

    tim_socketio.run(app, host="0.0.0.0", port=5000, debug=True)
