# -*- coding: utf-8 -*-

import http.client
import imghdr
import io
import os
import re
import shutil
import time
import traceback
from datetime import datetime
from datetime import timezone

import pprint
import werkzeug.exceptions as ex
from flask import Blueprint
from flask import Response
from flask import g, abort, flash
from flask import redirect
from flask import render_template
from flask import request
from flask import session
from flask import stream_with_context
from flask.helpers import send_file, url_for
from flask_assets import Environment
from markupsafe import Markup
from sqlalchemy.exc import IntegrityError
from werkzeug.contrib.profiler import ProfilerMiddleware

import containerLink
from ReverseProxied import ReverseProxied
from accesshelper import verify_admin, verify_edit_access, verify_manage_access, verify_view_access, \
    has_view_access, has_manage_access, grant_access_to_session_users, ItemLockedException, \
    get_viewable_blocks_or_none_if_admin
from cache import cache
from dbaccess import get_timdb
from documentmodel.document import Document
from documentmodel.documentversion import DocumentVersion
from logger import log_info, log_error, log_debug
from plugin import PluginException
from requesthelper import verify_json_params
from responsehelper import safe_redirect, json_response, ok_response
from routes.annotation import annotations
from routes.answer import answers
from routes.bookmarks import bookmarks
from routes.clipboard import clipboard
from routes.edit import edit_page
from routes.generateMap import generateMap
from routes.global_notification import global_notification
from routes.groups import groups
from routes.lecture import get_tempdb, user_in_lecture, lecture_routes
from routes.login import login_page, logout
from routes.manage import manage_page
from routes.notes import notes
from routes.notify import notify, send_email
from routes.qst import qst_plugin
from routes.readings import readings
from routes.print import print
from routes.search import search_routes
from routes.settings import settings_page
from routes.upload import upload
from routes.velp import velps
from routes.view import view_page
from sessioninfo import get_current_user_object, get_other_users_as_list, get_current_user_id, \
    get_current_user_name, get_current_user_group, logged_in
from tim_app import app
from timdb.blocktypes import from_str, blocktypes
from timdb.bookmarks import Bookmarks
from timdb.dbutils import copy_default_rights
from timdb.models.docentry import DocEntry
from timdb.models.folder import Folder
from timdb.models.translation import Translation
from timdb.models.user import User
from timdb.tim_models import db
from timdb.userutils import DOC_DEFAULT_RIGHT_NAME, FOLDER_DEFAULT_RIGHT_NAME, NoSuchUserException
from validation import validate_item_and_create

cache.init_app(app)

with app.app_context():
    cache.clear()

app.register_blueprint(generateMap)
app.register_blueprint(settings_page)
app.register_blueprint(manage_page)
app.register_blueprint(qst_plugin)
app.register_blueprint(edit_page)
app.register_blueprint(view_page)
app.register_blueprint(login_page)
app.register_blueprint(answers)
app.register_blueprint(velps)
app.register_blueprint(annotations)
app.register_blueprint(groups)
app.register_blueprint(search_routes)
app.register_blueprint(upload)
app.register_blueprint(notes)
app.register_blueprint(readings)
app.register_blueprint(lecture_routes)
app.register_blueprint(clipboard)
app.register_blueprint(notify)
app.register_blueprint(bookmarks)
app.register_blueprint(global_notification)
app.register_blueprint(print)
app.register_blueprint(Blueprint('bower',
                                 __name__,
                                 static_folder='static/scripts/bower_components',
                                 static_url_path='/static/scripts/bower_components'))

app.wsgi_app = ReverseProxied(app.wsgi_app)

assets = Environment(app)

log_info('Debug mode: {}'.format(app.config['DEBUG']))
log_info('Profiling: {}'.format(app.config['PROFILE']))


def error_generic(error, code):
    if 'text/html' in request.headers.get("Accept", ""):
        return render_template('error.html',
                               message=error.description,
                               code=code,
                               status=http.client.responses[code]), code
    else:
        return json_response({'error': error.description}, code)


@app.context_processor
def inject_custom_css() -> dict:
    """Injects the user prefs variable to all templates."""
    prefs = get_current_user_object().get_prefs()
    return dict(prefs=prefs)


@app.context_processor
def inject_user() -> dict:
    """"Injects the user object to all templates."""
    return dict(current_user=get_current_user_object(), other_users=get_other_users_as_list())


@app.context_processor
def inject_bookmarks() -> dict:
    """"Injects bookmarks to all templates."""
    if not logged_in():
        return {}
    return dict(bookmarks=Bookmarks(User.query.get(get_current_user_id())).as_dict())


@app.errorhandler(400)
def bad_request(error):
    return error_generic(error, 400)


# noinspection PyClassHasNoInit
class Forbidden(ex.HTTPException):
    code = 403
    description = "Sorry, you don't have permission to view this resource."


abort.mapping[403] = Forbidden


@app.errorhandler(Forbidden)
def forbidden(error):
    return error_generic(error, 403)


@app.errorhandler(500)
def internal_error(error):
    log_error(get_request_message(500, include_body=True))
    error.description = Markup('Something went wrong with the server, sorry. '
                               'TIM developers have been notified about this. '
                               'If the problem persists, please send email to <a href="mailto:{0}">{0}</a>.'
                               .format(app.config['HELP_EMAIL']))
    tb = traceback.format_exc()
    message = """
Exception happened on {} at {}

{}

{}
""".format(datetime.now(tz=timezone.utc),
           request.url,
           get_request_message(500, include_body=True),
           tb).strip()
    send_email(rcpt=app.config['ERROR_EMAIL'],
               subject='{}: Error at {} ({})'.format(app.config['TIM_HOST'], request.path, get_current_user_name()),
               mail_from=app.config['WUFF_EMAIL'],
               reply_to='{},{}'.format(app.config['ERROR_EMAIL'], get_current_user_object().email),
               msg=message)
    return error_generic(error, 500)


@app.route('/exception', methods=['GET', 'POST', 'PUT', 'DELETE'])
def throw_ex():
    verify_admin()
    raise Exception('This route throws an exception intentionally for testing purposes.')


@app.errorhandler(ItemLockedException)
def item_locked(error):
    item = DocEntry.find_by_id(error.access.block_id)
    is_folder = False
    if not item:
        is_folder = True
        item = Folder.get_by_id(error.access.block_id)
    if not item:
        abort(404)
    return render_template('duration_unlock.html',
                           item=item,
                           item_type='folder' if is_folder else 'document',
                           access=error.access), 403


@app.errorhandler(NoSuchUserException)
def internal_error(error):
    if error.user_id == session['user_id']:
        flash('Your user id ({}) was not found in the database. Clearing session automatically.'.format(error.user_id))
        return logout()
    return error_generic(error, 500)


@app.errorhandler(503)
def service_unavailable(error):
    return error_generic(error, 503)


@app.errorhandler(413)
def entity_too_large(error):
    error.description = 'Your file is too large to be uploaded. Maximum size is {} MB.'\
        .format(app.config['MAX_CONTENT_LENGTH'] / 1024 / 1024)
    return error_generic(error, 413)


@app.errorhandler(404)
def not_found(error):
    return error_generic(error, 404)


@app.route('/restart')
def restart_server():
    """Restarts the server by sending HUP signal to Gunicorn."""
    verify_admin()
    pid_path = '/var/run/gunicorn.pid'
    if os.path.exists(pid_path):
        os.system('kill -HUP $(cat {})'.format(pid_path))
        flash('Restart signal was sent to Gunicorn.')
    else:
        flash('Gunicorn PID file was not found. TIM was probably not started with Gunicorn.')
    return safe_redirect(url_for('start_page'))


@app.route('/resetcss')
def reset_css():
    """Removes CSS cache directories and thereby forces SASS to regenerate them the next time they are needed.

    Requires admin privilege.
    :return: ok_response

    """
    verify_admin()
    assets_dir = os.path.join('static', '.webassets-cache')
    if os.path.exists(assets_dir):
        shutil.rmtree(assets_dir)
    gen_dir = os.path.join('static', app.config['SASS_GEN_PATH'])
    if os.path.exists(gen_dir):
        shutil.rmtree(gen_dir)
    return safe_redirect(url_for('start_page'))


@app.route('/download/<int:doc_id>')
def download_document(doc_id):
    verify_edit_access(doc_id, "Sorry, you don't have permission to download this document.")
    return Response(Document(doc_id).export_markdown(), mimetype="text/plain")


@app.route('/download/<int:doc_id>/<int:major>/<int:minor>')
def download_document_version(doc_id, major, minor):
    verify_edit_access(doc_id)
    doc = DocumentVersion(doc_id, (major, minor))
    if not doc.exists():
        abort(404, "This document version does not exist.")
    return Response(doc.export_markdown(), mimetype="text/plain")


@app.route('/diff/<int:doc_id>/<int:major1>/<int:minor1>/<int:major2>/<int:minor2>')
def diff_document(doc_id, major1, minor1, major2, minor2):
    verify_edit_access(doc_id)
    doc1 = DocumentVersion(doc_id, (major1, minor1))
    doc2 = DocumentVersion(doc_id, (major2, minor2))
    if not doc1.exists():
        abort(404, "The document version {} does not exist.".format((major1, minor1)))
    if not doc2.exists():
        abort(404, "The document version {} does not exist.".format((major2, minor2)))
    return Response(DocumentVersion.get_diff(doc1, doc2), mimetype="text/html")


@app.route('/images/<int:image_id>/<image_filename>')
def get_image(image_id, image_filename):
    timdb = get_timdb()
    if not timdb.images.imageExists(image_id, image_filename):
        abort(404)
    verify_view_access(image_id)
    img_data = timdb.images.getImage(image_id, image_filename)
    imgtype = imghdr.what(None, h=img_data)
    f = io.BytesIO(img_data)
    return send_file(f, mimetype='image/' + imgtype)


@app.route("/getTemplates")
def get_templates():
    current_path = request.args.get('item_path', '')
    timdb = get_timdb()
    templates = []
    doc = DocEntry.find_by_path(current_path, try_translation=True)
    if not doc:
        abort(404)
    verify_edit_access(doc.id)
    current_path = doc.parent.path

    while True:
        for t in timdb.documents.get_documents(filter_ids=get_viewable_blocks_or_none_if_admin(),
                                               filter_folder=current_path + '/Templates',
                                               search_recursively=False):
            if t.short_name not in (DOC_DEFAULT_RIGHT_NAME, FOLDER_DEFAULT_RIGHT_NAME):
                templates.append(t)
        if current_path == '':
            break
        current_path, _ = timdb.folders.split_location(current_path)
    templates.sort(key=lambda d: d.short_name.lower())
    return json_response(templates)


def create_item(item_path, item_type_str, item_title, create_function, owner_group_id):
    item_path = item_path.strip('/')
    validate_item_and_create(item_path, item_type_str, owner_group_id)

    item = create_function(item_path, owner_group_id, item_title)
    timdb = get_timdb()
    grant_access_to_session_users(timdb, item.id)
    item_type = from_str(item_type_str)
    if item_type == blocktypes.DOCUMENT:
        bms = Bookmarks(get_current_user_object())
        bms.add_bookmark('Last edited',
                         item.title,
                         '/view/' + item.path,
                         move_to_top=True,
                         limit=app.config['LAST_EDITED_BOOKMARK_LIMIT']).save_bookmarks()
    copy_default_rights(item.id, item_type)
    return item


@app.route("/createItem", methods=["POST"])
def create_document():
    item_path, item_type, item_title = verify_json_params('item_path', 'item_type', 'item_title')
    cite_id, copy_id = verify_json_params('cite', 'copy', require=False)
    if cite_id:
        return create_citation_doc(cite_id, item_path, item_title)

    copied_content = None
    if copy_id:
        verify_edit_access(copy_id)
        d = DocEntry.find_by_id(copy_id, try_translation=True)
        if not d:
            return abort(404, 'The document to be copied was not found')
        copied_content = d.document.export_markdown()

    item = create_item(item_path,
                       item_type,
                       item_title,
                       DocEntry.create if item_type == 'document' else Folder.create,
                       get_current_user_group())

    if copied_content:
        item.document.update(copied_content, item.document.export_markdown())

    return json_response(item)


@app.route("/translations/<int:doc_id>", methods=["GET"])
def get_translations(doc_id):
    timdb = get_timdb()

    if not timdb.documents.exists(doc_id):
        abort(404, 'Document not found')
    verify_manage_access(doc_id)

    return json_response(DocEntry.find_by_id(doc_id, try_translation=True).translations)


def valid_language_id(lang_id):
    return re.match('^\w+$', lang_id) is not None


@app.route("/translate/<int:tr_doc_id>/<language>", methods=["POST"])
def create_translation(tr_doc_id, language):
    title = request.get_json().get('doc_title', None)
    timdb = get_timdb()

    doc = DocEntry.find_by_id(tr_doc_id, try_translation=True)

    if not doc:
        abort(404, 'Document not found')

    doc_id = doc.src_docid

    if not has_view_access(doc_id):
        abort(403, 'Permission denied')
    if not valid_language_id(language):
        abort(404, 'Invalid language identifier')
    if doc.has_translation(language):
        abort(403, 'Translation for this language already exists')
    verify_manage_access(doc_id)

    src_doc = Document(doc_id)
    cite_doc = timdb.documents.create_citation(src_doc, get_current_user_group())
    # noinspection PyArgumentList
    tr = Translation(doc_id=cite_doc.id, src_docid=src_doc.doc_id, lang_id=language)
    tr.title = title
    db.session.add(tr)
    db.session.commit()
    return json_response(tr)


@app.route("/translation/<int:doc_id>", methods=["POST"])
def update_translation(doc_id):
    (lang_id, doc_title) = verify_json_params('new_langid', 'new_title', require=True)
    if not valid_language_id(lang_id):
        abort(403, 'Invalid language identifier')
    doc = DocEntry.find_by_id(doc_id, try_translation=True)
    if not doc:
        abort(404, 'Source document does not exist')
    if not has_manage_access(doc.src_docid) and not has_manage_access(doc.id):
        abort(403, "You need manage access of either this or the translated document")
    doc.lang_id = lang_id
    doc.title = doc_title
    try:
        db.session.commit()
    except IntegrityError:
        abort(403, 'This language already exists.')
    return ok_response()


def create_citation_doc(doc_id, doc_path, doc_title):
    params, = verify_json_params('params', require=False)

    # Filter for allowed reference parameters
    if params is not None:
        params = {k: params[k] for k in params if k in ('r', 'r_docid')}
        params['r'] = 'c'
    else:
        params = {'r': 'c'}

    timdb = get_timdb()
    verify_edit_access(doc_id)

    src_doc = Document(doc_id)

    def factory(path, group, title):
        return timdb.documents.create_citation(src_doc, group, path, title, params)
    item = create_item(doc_path, 'document', doc_title, factory, get_current_user_group())
    return json_response(item)


@app.route("/getBlock/<int:doc_id>/<par_id>")
def get_block(doc_id, par_id):
    verify_edit_access(doc_id)
    area_start = request.args.get('area_start')
    area_end = request.args.get('area_end')
    if area_start and area_end:
        return json_response({"text": Document(doc_id).export_section(area_start, area_end)})
    else:
        par = Document(doc_id).get_paragraph(par_id)
        return json_response({"text": par.get_exported_markdown()})


@app.route("/<plugin>/<path:filename>")
def plugin_call(plugin, filename):
    try:
        req = containerLink.call_plugin_resource(plugin, filename, request.args)
        return Response(stream_with_context(req.iter_content()), content_type=req.headers['content-type'])
    except PluginException:
        abort(404)


# noinspection PyUnusedLocal
@app.route("/echoRequest/<path:filename>")
def echo_request(filename):
    def generate():
        yield 'Request URL: ' + request.url + "\n\n"
        yield 'Headers:\n\n'
        yield from (k + ": " + v + "\n" for k, v in request.headers.items())
    return Response(stream_with_context(generate()), mimetype='text/plain')


@app.route("/index/<int:doc_id>")
def get_index(doc_id):
    verify_view_access(doc_id)
    index = Document(doc_id).get_index()
    if not index:
        return json_response({'empty': True})
    else:
        return render_template('partials/content.html',
                               headers=index)


@app.route("/<plugin>/template/<template>/<index>")
def view_template(plugin, template, index):
    try:
        req = containerLink.call_plugin_resource(plugin, "template?file=" + template + "&idx=" + index)
        return Response(stream_with_context(req.iter_content()), content_type=req.headers['content-type'])
    except PluginException:
        abort(404)


@app.route("/sessionsetting/<setting>/<value>", methods=['POST'])
def set_session_setting(setting, value):
    try:
        if 'settings' not in session:
            session['settings'] = {}
        session['settings'][setting] = value
        session.modified = True
        return json_response(session['settings'])
    except (NameError, KeyError):
        abort(404)


@app.route("/getServerTime", methods=['GET'])
def get_server_time():
    t2 = int(time.time() * 1000)
    t1 = int(request.args.get('t1'))
    return json_response({'t1': t1, 't2': t2, 't3': int(time.time() * 1000)})


@app.route("/")
def start_page():
    in_lecture = user_in_lecture()
    return render_template('start.html',
                           in_lecture=in_lecture)


@app.route("/manage/")
@app.route("/slide/")
@app.route("/teacher/")
@app.route("/answers/")
@app.route("/lecture/")
def index_redirect():
    return redirect('/view')


@app.route("/getslidestatus/")
def getslidestatus():
    if 'doc_id' not in request.args:
        abort(404, "Missing doc id")
    doc_id = int(request.args['doc_id'])
    tempdb = get_tempdb()
    status = tempdb.slidestatuses.get_status(doc_id)
    if status:
        status = status.status
    else:
        status = None
    return json_response(status)


@app.route("/setslidestatus")
def setslidestatus():
    if 'doc_id' not in request.args or 'status' not in request.args:
        abort(404, "Missing doc id or status")
    doc_id = int(request.args['doc_id'])
    verify_manage_access(doc_id)
    status = request.args['status']
    tempdb = get_tempdb()
    tempdb.slidestatuses.update_or_add_status(doc_id, status)
    return json_response("")


@app.before_request
def make_session_permanent():
    session.permanent = True


@app.after_request
def log_request(response):
    if not request.path.startswith('/static/') and request.path != '/favicon.ico':
        status_code = response.status_code
        log_info(get_request_message(status_code))
        if request.method in ('PUT', 'POST', 'DELETE'):
            log_debug(request.get_json(silent=True))
    return response


def get_request_message(status_code=None, include_body=False):
    msg = '{} [{}]: {} {} {}'.format(get_current_user_name(),
                                     request.headers.get('X-Forwarded-For') or request.remote_addr,
                                     request.method,
                                     request.full_path if request.query_string else request.path,
                                     status_code or '').strip()
    if not include_body or request.method not in ('POST', 'PUT', 'DELETE'):
        return msg
    return '{}\n\n{}'.format(msg, pprint.pformat(request.get_json(silent=True) or request.get_data(as_text=True)))


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


# noinspection PyUnusedLocal
@app.teardown_appcontext
def close_db(e):
    if not app.config['TESTING'] and hasattr(g, 'timdb'):
        g.timdb.close()


def start_app():
    if app.config['PROFILE']:
        app.wsgi_app = ProfilerMiddleware(app.wsgi_app, sort_by=('cumtime',), restrictions=[100])
    app.run(host='0.0.0.0',
            port=5000,
            use_evalex=False,
            use_reloader=False,
            # debug=True,
            threaded=not (app.config['DEBUG'] and app.config['PROFILE']))
