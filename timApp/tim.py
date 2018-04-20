# -*- coding: utf-8 -*-

import http.client
import imghdr
import io
import os
import pprint
import re
import shutil
import time
import traceback
from datetime import datetime
from datetime import timezone

import werkzeug.exceptions as ex
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

from timApp.ReverseProxied import ReverseProxied
from timApp.accesshelper import verify_admin, verify_edit_access, verify_manage_access, verify_view_access, \
    has_manage_access, ItemLockedException, get_doc_or_abort
from timApp.cache import cache
from timApp.containerLink import call_plugin_resource
from timApp.dbaccess import get_timdb
from timApp.documentmodel.create_item import get_templates_for_folder, create_or_copy_item
from timApp.documentmodel.document import Document
from timApp.documentmodel.documentversion import DocumentVersion
from timApp.logger import log_info, log_error, log_debug, log_warning
from timApp.minutes.minuteroutes import minutes_blueprint
from timApp.pluginexception import PluginException
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import safe_redirect, json_response, ok_response
from timApp.routes.annotation import annotations
from timApp.routes.answer import answers
from timApp.routes.bookmarks import bookmarks
from timApp.routes.clipboard import clipboard
from timApp.routes.edit import edit_page
from timApp.routes.generateMap import generateMap
from timApp.routes.global_notification import global_notification
from timApp.routes.groups import groups
from timApp.routes.lecture import get_tempdb, lecture_routes
from timApp.routes.login import login_page, logout
from timApp.routes.manage import manage_page
from timApp.routes.notes import notes
from timApp.routes.notify import notify, send_email
from timApp.routes.print import print_blueprint
from timApp.routes.qst import qst_plugin
from timApp.plugins.timTable import timTable_plugin
from timApp.routes.readings import readings
from timApp.routes.search import search_routes
from timApp.routes.settings import settings_page
from timApp.routes.static_3rdparty import static_blueprint
from timApp.routes.upload import upload
from timApp.routes.velp import velps
from timApp.routes.view import view_page
from timApp.sessioninfo import get_current_user_object, get_other_users_as_list, get_current_user_id, \
    get_current_user_name, get_current_user_group, logged_in, current_user_in_lecture
from timApp.tim_app import app, default_secret
from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.bookmarks import Bookmarks
from timApp.timdb.documents import create_translation
from timApp.timdb.exceptions import TimDbException, ItemAlreadyExistsException
from timApp.timdb.item import Item
from timApp.timdb.models.block import copy_default_rights
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.translation import Translation
from timApp.timdb.models.user import User
from timApp.timdb.tim_models import db
from timApp.timdb.userutils import NoSuchUserException

# for pdf merging
import timApp.tools.pdftools

import timApp.plugin

cache.init_app(app)

app.register_blueprint(generateMap)
app.register_blueprint(settings_page)
app.register_blueprint(manage_page)
app.register_blueprint(qst_plugin)
app.register_blueprint(timTable_plugin)
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
app.register_blueprint(static_blueprint)
app.register_blueprint(print_blueprint)
app.register_blueprint(minutes_blueprint)

app.wsgi_app = ReverseProxied(app.wsgi_app)

assets = Environment(app)


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


@app.errorhandler(400)
def bad_request(error):
    return error_generic(error, 400)


# noinspection PyClassHasNoInit
class Forbidden(ex.HTTPException):
    code = 403
    description = "Sorry, you don't have permission to view this resource."


ex._aborter.mapping[403] = Forbidden


@app.errorhandler(Forbidden)
def forbidden(error):
    return error_generic(error, 403)


@app.errorhandler(ItemAlreadyExistsException)
def already_exists(error: ItemAlreadyExistsException):
    return error_generic(Forbidden(description=str(error)), 403)


@app.errorhandler(500)
def internal_error(error):
    log_error(get_request_message(500, include_body=True))
    help_email = app.config['HELP_EMAIL']
    error.description = Markup('Something went wrong with the server, sorry. '
                               'TIM developers have been notified about this. '
                               f'If the problem persists, please send email to <a href="mailto:{help_email}">{help_email}</a>.')
    tb = traceback.format_exc()
    message = f"""
Exception happened on {datetime.now(tz=timezone.utc)} at {request.url}

{get_request_message(500, include_body=True)}

{tb}
""".strip()
    send_email(rcpt=app.config['ERROR_EMAIL'],
               subject=f'{app.config["TIM_HOST"]}: Error at {request.path} ({get_current_user_name()})',
               mail_from=app.config['WUFF_EMAIL'],
               reply_to=f'{app.config["ERROR_EMAIL"]},{get_current_user_object().email}',
               msg=message)
    return error_generic(error, 500)


@app.route('/empty')
def empty_response_route():
    return Response('', mimetype='text/plain')


@app.route('/mergeAttachments/<path:doc>', methods=['GET'])
def get_attachments(doc):
    """
    A route for getting merged document.
    :param doc: document path
    :return: merged pdf-file
    """

    # TODO: Check if there is PDF all ready
    try:
        d = DocEntry.find_by_path(doc, try_translation=True)
        if not d:
            abort(404)
        verify_edit_access(d)

        paragraphs = d.document.get_paragraphs(d)
        pdf_paths = []

        for par in paragraphs:
            if par.is_plugin() and par.get_attr('plugin') == 'showPdf':
                par_plugin = timApp.plugin.Plugin.from_paragraph(par)
                par_data = par_plugin.values
                par_file = par_data["file"]

                # checks if attachment is TIM-upload and adds prefix
                # changes in upload location need to be updated here as well!
                if par_file.startswith("/files/"):
                    pdf_paths += ["/tim_files/blocks" + par_file]

                # if attachment is url link, download it to temp folder to operate
                elif timApp.tools.pdftools.is_url(par_file):
                    # print(f"Downloading {par_file}")
                    pdf_paths += [timApp.tools.pdftools.download_file_from_url(par_file)]

        # uses document name as the base for the merged file name and tmp as folder
        doc_name = timApp.tools.pdftools.get_base_filename(doc)
        merged_pdf_path = timApp.tools.pdftools.temp_folder_default_path + "/" + f"{doc_name}_merged.pdf"
        timApp.tools.pdftools.merge_pdf(pdf_paths, merged_pdf_path)

    except Exception as e:
        message = str(e)
        print(message)
        abort(404, message)
    else:
       return send_file(merged_pdf_path, mimetype="application/pdf")

@app.route('/mergeAttachments/<path:doc>', methods=['POST'])
def merge_attachments(doc):
    """
    A route for merging all the attachments in a document.
    :param doc: document path
    :return: merged pdf-file
    """
    try:
        d = DocEntry.find_by_path(doc, try_translation=True)
        if not d:
            abort(404)
        verify_edit_access(d)

    except Exception as e:
        message = str(e)
        abort(404, message)
    else:
        merge_access_url = request.url
        return json_response({'success': True, 'url': merge_access_url}, status_code=201)

@app.route('/processAttachments/<path:doc>')
def process_attachments(doc):
    """
    A testing route for processing pdf attachments.
    Note: possibly obsolete.
    :param doc:
    :return: merged & stamped pdf
    """
    # TODO: divide try-block a bit (use PdfError for pdftools methods)
    try:
        d = DocEntry.find_by_path(doc, try_translation=True)
        if not d:
            abort(404)
        verify_view_access(d)

        paragraphs = d.document.get_paragraphs(d)
        pdf_stamp_data = []
        for par in paragraphs:
            if par.is_plugin() and par.get_attr('plugin') == 'showPdf':
                par_plugin = timApp.plugin.Plugin.from_paragraph(par)
                par_data = par_plugin.values
                par_file = par_data["file"]

                # checks if attachment is TIM-upload and adds prefix
                if par_file.startswith("/files/"):
                    par_file = "/tim_files/blocks" + par_file

                # if attachment is url link, download it to temp folder and operate the
                # downloaded file
                if timApp.tools.pdftools.is_url(par_file):
                    print(f"Downloading {par_file}")
                    par_data["file"] = timApp.tools.pdftools.download_file_from_url(par_file)

                pdf_stamp_data += [par_data]
                print(repr(par_data))

        # now separate stamp and merge
        stamped_pdfs = timApp.tools.pdftools.stamp_pdfs(pdf_stamp_data)
        print(stamped_pdfs)

        # uses document name as the base for the merged file name
        merged_pdf_path = f"static/testpdf/{doc}_merged.pdf"
        timApp.tools.pdftools.merge_pdf(stamped_pdfs, merged_pdf_path)

    except Exception as e:
        message = repr(e)
        abort(404, message)
    else:
        # TODO: do something more intelligent here
        return send_file(merged_pdf_path, mimetype="application/pdf")


@app.route('/exception', methods=['GET', 'POST', 'PUT', 'DELETE'])
def throw_ex():
    verify_admin()
    raise Exception('This route throws an exception intentionally for testing purposes.')


@app.errorhandler(ItemLockedException)
def item_locked(error: ItemLockedException):
    item = DocEntry.find_by_id(error.access.block_id, try_translation=True)
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
def handle_user_not_found(error):
    if error.user_id == session['user_id']:
        flash(f'Your user id ({error.user_id}) was not found in the database. Clearing session automatically.')
        return logout()
    return error_generic(error, 500)


@app.errorhandler(503)
def service_unavailable(error):
    return error_generic(error, 503)


@app.errorhandler(413)
def entity_too_large(error):
    error.description = f'Your file is too large to be uploaded. Maximum size is {app.config["MAX_CONTENT_LENGTH"] / 1024 / 1024} MB.'
    return error_generic(error, 413)


@app.errorhandler(404)
def not_found(error):
    return error_generic(error, 404)


@app.route("/ping")
def ping():
    return ok_response()


@app.route('/restart')
def restart_server():
    """Restarts the server by sending HUP signal to Gunicorn."""
    verify_admin()
    pid_path = '/var/run/gunicorn.pid'
    if os.path.exists(pid_path):
        os.system(f'kill -HUP $(cat {pid_path})')
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
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    return Response(d.document.export_markdown(), mimetype="text/plain")


@app.route('/download/<int:doc_id>/<int:major>/<int:minor>')
def download_document_version(doc_id, major, minor):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    doc = DocumentVersion(doc_id, (major, minor))
    if not doc.exists():
        abort(404, "This document version does not exist.")
    return Response(doc.export_markdown(), mimetype="text/plain")


@app.route('/diff/<int:doc_id>/<int:major1>/<int:minor1>/<int:major2>/<int:minor2>')
def diff_document(doc_id, major1, minor1, major2, minor2):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    doc1 = DocumentVersion(doc_id, (major1, minor1))
    doc2 = DocumentVersion(doc_id, (major2, minor2))
    if not doc1.exists():
        abort(404, f"The document version {(major1, minor1)} does not exist.")
    if not doc2.exists():
        abort(404, f"The document version {(major2, minor2)} does not exist.")
    return Response(DocumentVersion.get_diff(doc1, doc2), mimetype="text/html")


@app.route('/images/<int:image_id>/<image_filename>')
def get_image(image_id, image_filename):
    timdb = get_timdb()
    b = timdb.images.get_image_block(image_id, image_filename)
    if not b:
        abort(404)
    verify_view_access(b)
    img_data = timdb.images.getImage(image_id, image_filename)
    imgtype = imghdr.what(None, h=img_data)
    f = io.BytesIO(img_data)
    return send_file(f, mimetype='image/' + imgtype)


@app.route("/getTemplates")
def get_templates():
    current_path = request.args.get('item_path', '')
    d = DocEntry.find_by_path(current_path, try_translation=True)
    if not d:
        abort(404)
    verify_edit_access(d)
    templates = get_templates_for_folder(d.parent)
    return json_response(templates)


@app.route("/createItem", methods=["POST"])
def create_item_route():
    item_path, item_type, item_title = verify_json_params('item_path', 'item_type', 'item_title')
    cite_id, copy_id, template_name, use_template = verify_json_params('cite', 'copy', 'template', 'use_template',
                                                                       require=False)

    if use_template is None:
        use_template = True

    item = create_or_copy_item(item_path, item_type, item_title, cite_id, copy_id, template_name, use_template)
    db.session.commit()
    return json_response(item)


@app.route("/translations/<int:doc_id>", methods=["GET"])
def get_translations(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)

    return json_response(d.translations)


def valid_language_id(lang_id):
    return re.match('^\w+$', lang_id) is not None


@app.route("/translate/<int:tr_doc_id>/<language>", methods=["POST"])
def create_translation_route(tr_doc_id, language):
    title = request.get_json().get('doc_title', None)

    doc = get_doc_or_abort(tr_doc_id)

    verify_view_access(doc)
    if not valid_language_id(language):
        abort(404, 'Invalid language identifier')
    if doc.has_translation(language):
        raise ItemAlreadyExistsException('Translation for this language already exists')
    verify_manage_access(doc.src_doc)

    src_doc = doc.src_doc.document
    cite_doc = create_translation(src_doc, get_current_user_group())
    # noinspection PyArgumentList
    tr = Translation(doc_id=cite_doc.doc_id, src_docid=src_doc.doc_id, lang_id=language)
    tr.title = title
    db.session.add(tr)
    copy_default_rights(cite_doc.doc_id, blocktypes.DOCUMENT)
    db.session.commit()
    return json_response(tr)


@app.route("/translation/<int:doc_id>", methods=["POST"])
def update_translation(doc_id):
    (lang_id, doc_title) = verify_json_params('new_langid', 'new_title', require=True)
    if not valid_language_id(lang_id):
        abort(403, 'Invalid language identifier')
    doc = get_doc_or_abort(doc_id)
    if not has_manage_access(doc) and not has_manage_access(doc):
        abort(403, "You need manage access of either this or the translated document")
    doc.lang_id = lang_id
    doc.title = doc_title
    try:
        db.session.commit()
    except IntegrityError:
        raise ItemAlreadyExistsException('This language already exists.')
    return ok_response()


@app.route("/getBlock/<int:doc_id>/<par_id>")
def get_block(doc_id, par_id):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    area_start = request.args.get('area_start')
    area_end = request.args.get('area_end')
    if area_start and area_end:
        try:
            section = d.document.export_section(area_start, area_end)
        except TimDbException as e:
            return abort(404, str(e))
        return json_response({"text": section})
    else:
        try:
            par = d.document.get_paragraph(par_id)
        except TimDbException as e:
            return abort(404, str(e))
        return json_response({"text": par.get_exported_markdown()})


@app.route("/items/<int:item_id>")
def get_item(item_id: int):
    i = Item.find_by_id(item_id)
    if not i:
        abort(404, 'Item not found')
    verify_view_access(i)
    return json_response(i)


@app.route("/<plugin>/<path:filename>")
def plugin_call(plugin, filename):
    try:
        req = call_plugin_resource(plugin, filename, request.args)
        return Response(stream_with_context(req.iter_content()), content_type=req.headers['content-type'])
    except PluginException as e:
        log_warning(str(e))
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
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    index = Document(doc_id).get_index()
    if not index:
        return json_response({'empty': True})
    else:
        return render_template('partials/content.html',
                               headers=index)


@app.route("/<plugin>/template/<template>/<index>")
def view_template(plugin, template, index):
    try:
        req = call_plugin_resource(plugin, "template?file=" + template + "&idx=" + index)
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
    in_lecture = current_user_in_lecture()
    return render_template('start.html',
                           in_lecture=in_lecture)


@app.route("/manage")
@app.route("/slide")
@app.route("/teacher")
@app.route("/answers")
@app.route("/lecture")
def index_redirect():
    return redirect('/view')


@app.route("/getslidestatus")
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
    d = get_doc_or_abort(doc_id)
    verify_manage_access(d)
    status = request.args['status']
    tempdb = get_tempdb()
    tempdb.slidestatuses.update_or_add_status(doc_id, status)
    return ok_response()


@app.before_request
def preprocess_request():
    session.permanent = True
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


def get_request_message(status_code=None, include_body=False):
    msg = f'{get_current_user_name()} [{request.headers.get("X-Forwarded-For") or request.remote_addr}]: {request.method} {request.full_path if request.query_string else request.path} {status_code or ""}'.strip()
    if not include_body or request.method not in ('POST', 'PUT', 'DELETE'):
        return msg
    return f'{msg}\n\n{pprint.pformat(request.get_json(silent=True) or request.get_data(as_text=True))}'


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
