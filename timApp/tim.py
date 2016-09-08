# -*- coding: utf-8 -*-

import http.client
import imghdr
import io
import time

import shutil
from flask import Blueprint
from flask import render_template
from flask import stream_with_context
from flask_assets import Environment
from flask.helpers import send_file
from werkzeug.contrib.profiler import ProfilerMiddleware

import containerLink
from ReverseProxied import ReverseProxied
from documentmodel.documentversion import DocumentVersion
from plugin import PluginException
from routes.answer import answers
from routes.velp import velps
from routes.annotation import annotations
from routes.cache import cache
from routes.clipboard import clipboard
from routes.common import *
from routes.edit import edit_page
from routes.groups import groups
from routes.lecture import getTempDb, user_in_lecture, lecture_routes
from routes.logger import log_info
from routes.login import login_page, logout
from routes.manage import manage_page
from routes.notes import notes
from routes.notify import notify
from routes.readings import readings
from routes.search import search_routes
from routes.settings import settings_page
from routes.upload import upload
from routes.view import view_page
from tim_app import app

# db.engine.pool.use_threadlocal = True # This may be needless
from utils import date_to_relative
from timdb.users import NoSuchUserException

cache.init_app(app)

with app.app_context():
    cache.clear()

app.register_blueprint(settings_page)
app.register_blueprint(manage_page)
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
        return jsonResponse({'error': error.description}, code)


@app.context_processor
def inject_custom_css() -> dict:
    """Injects the user prefs variable to all templates."""
    prefs = get_preferences()
    return dict(prefs=prefs)


@app.context_processor
def inject_user() -> dict:
    """"Injects the user object to all templates."""
    return dict(current_user=get_current_user(), other_users=get_other_users_as_list())


@app.errorhandler(400)
def bad_request(error):
    return error_generic(error, 400)


@app.errorhandler(403)
def forbidden(error):
    return error_generic(error, 403)


@app.errorhandler(500)
def internal_error(error):
    error.description = "Something went wrong with the server, sorry. We'll fix this as soon as possible."
    return error_generic(error, 500)


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


@app.route('/resetcss')
def reset_css():
    """
    Removes CSS cache directories and thereby forces SASS to regenerate them the next time
    they are needed.

    Requires admin privilege.
    :return: okJsonResponse
    """
    verify_admin()
    assets_dir = os.path.join('static', '.webassets-cache')
    if os.path.exists(assets_dir):
        shutil.rmtree(assets_dir)
    gen_dir = os.path.join('static', app.config['SASS_GEN_PATH'])
    if os.path.exists(gen_dir):
        shutil.rmtree(gen_dir)
    return okJsonResponse()


@app.route('/download/<int:doc_id>')
def download_document(doc_id):
    verify_doc_existence(doc_id)
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
    timdb = getTimDb()
    if not timdb.images.imageExists(image_id, image_filename):
        abort(404)
    verify_view_access(image_id)
    img_data = timdb.images.getImage(image_id, image_filename)
    imgtype = imghdr.what(None, h=img_data)
    f = io.BytesIO(img_data)
    return send_file(f, mimetype='image/' + imgtype)


@app.route("/getDocuments")
def get_docs():
    return jsonResponse(get_documents(request.args.get('folder')))


@app.route("/getFolders")
def get_folders():
    root_path = request.args.get('root_path')
    timdb = getTimDb()
    folders = timdb.folders.get_folders(root_path)
    viewable = timdb.users.get_viewable_blocks(getCurrentUserId())
    allowed_folders = [f for f in folders if f['id'] in viewable]
    uid = getCurrentUserId()
    is_admin = timdb.users.has_admin_access(uid)
    for f in allowed_folders:
        f['isOwner'] = is_admin or timdb.users.user_is_owner(uid, f['id'])
        f['owner'] = timdb.users.get_owner_group(f['id'])

    allowed_folders.sort(key=lambda folder: folder['name'].lower())
    return jsonResponse(allowed_folders)


@app.route("/getTemplates")
def get_templates():
    current_path = request.args.get('root_path')
    timdb = getTimDb()
    templates = []

    while True:
        if current_path != '':
            found_templates = get_documents(current_path + '/Templates')
        else:
            found_templates = get_documents('Templates')
        for t in found_templates:
            if timdb.users.has_manage_access(getCurrentUserId(), timdb.documents.get_document_id(t['fullname'])):
                templates.append(t)
        if current_path == '':
            break
        current_path, _ = timdb.folders.split_location(current_path)

    return jsonResponse(templates)


def get_documents(folder):
    timdb = getTimDb()
    docs = timdb.documents.get_documents()
    viewable = timdb.users.get_viewable_blocks(getCurrentUserId())
    allowed_docs = [doc for doc in docs if doc['id'] in viewable]

    req_folder = folder
    if req_folder is not None and len(req_folder) == 0:
        req_folder = None
    final_docs = []

    for doc in allowed_docs:
        fullname = doc['name']

        if req_folder:
            if not fullname.startswith(req_folder + '/'):
                continue
            docname = fullname[len(req_folder) + 1:]
        else:
            docname = fullname

        if '/' in docname:
            continue

        uid = getCurrentUserId()
        doc['name'] = docname
        doc['fullname'] = fullname
        doc['canEdit'] = timdb.users.has_edit_access(uid, doc['id'])
        doc['isOwner'] = timdb.users.user_is_owner(getCurrentUserId(), doc['id'])
        doc['owner'] = timdb.users.get_owner_group(doc['id'])
        doc['modified'] = date_to_relative(doc['modified'])
        final_docs.append(doc)

    final_docs.sort(key=lambda d: d['name'].lower())
    return final_docs


def create_item(item_name, item_type, create_function, owner_group_id):
    validate_item_and_create(item_name, item_type, owner_group_id)

    item_id = create_function(item_name, owner_group_id)
    grant_access_to_session_users(getTimDb(), item_id)
    return jsonResponse({'id': item_id, 'name': item_name})


@app.route("/createDocument", methods=["POST"])
def create_document():
    jsondata = request.get_json()
    doc_name = jsondata['doc_name']

    timdb = getTimDb()
    return create_item(doc_name, 'document', lambda name, group: timdb.documents.create(name, group).doc_id,
                       getCurrentUserGroup())


@app.route("/translations/<int:doc_id>", methods=["GET"])
def get_translations(doc_id):
    timdb = getTimDb()

    if not timdb.documents.exists(doc_id):
        abort(404, 'Document not found')
    if not has_view_access(doc_id):
        abort(403, 'Permission denied')

    trlist = timdb.documents.get_translations(doc_id)
    for tr in trlist:
        tr['owner'] = timdb.users.get_user_group_name(tr['owner_id']) if tr['owner_id'] else None

    return jsonResponse(trlist)


def valid_language_id(lang_id):
    return re.match('^\w+$', lang_id) is not None


@app.route("/translate/<int:tr_doc_id>/<language>", methods=["POST"])
def create_translation(tr_doc_id, language):
    title = request.get_json().get('doc_title', None)
    timdb = getTimDb()

    doc_id = timdb.documents.get_translation_source(tr_doc_id)

    if not timdb.documents.exists(doc_id):
        abort(404, 'Document not found')

    if not has_view_access(doc_id):
        abort(403, 'Permission denied')
    if not valid_language_id(language):
        abort(404, 'Invalid language identifier')
    if timdb.documents.translation_exists(doc_id, lang_id=language):
        abort(403, 'Translation already exists')
    if not has_manage_access(doc_id):
        # todo: check for translation right
        abort(403, 'You have to be logged in to create a translation')

    src_doc = Document(doc_id)
    doc = timdb.documents.create_translation(src_doc, None, getCurrentUserGroup())
    timdb.documents.add_translation(doc.doc_id, src_doc.doc_id, language, title)

    src_doc_name = timdb.documents.get_first_document_name(src_doc.doc_id)
    doc_name = timdb.documents.get_translation_path(doc_id, src_doc_name, language)

    return jsonResponse({'id': doc.doc_id, 'title': title, 'name': doc_name})


@app.route("/translation/<int:doc_id>", methods=["POST"])
def update_translation(doc_id):
    (lang_id, doc_title) = verify_json_params('new_langid', 'new_title', require=True)
    timdb = getTimDb()

    src_doc_id = doc_id
    translations = timdb.documents.get_translations(doc_id)
    for tr in translations:
        if tr['id'] == doc_id:
            src_doc_id = tr['src_docid']
        if tr['lang_id'] == lang_id and tr['id'] != doc_id:
            abort(403, 'Translation ' + lang_id + ' already exists')

    if src_doc_id is None or not timdb.documents.exists(src_doc_id):
        abort(404, 'Source document does not exist')

    if not valid_language_id(lang_id):
        if doc_id == src_doc_id and lang_id == "":
            # Allow removing the language id for the document itself
            timdb.documents.remove_translation(doc_id)

            # Rename the document if requested
            (prev_title,) = verify_json_params('old_title', require=False)
            if prev_title is not None and doc_title != prev_title:
                timdb.documents.change_name(doc_id, prev_title, doc_title)

            return okJsonResponse()

        abort(403, 'Invalid language identifier')

    if not has_ownership(src_doc_id) and not has_ownership(doc_id):
        abort(403, "You need ownership of either this or the translated document")

    # Remove and add because we might be adding a language identifier for the source document
    # In that case there may be nothing to update!
    timdb.documents.remove_translation(doc_id, commit=False)
    timdb.documents.add_translation(doc_id, src_doc_id, lang_id, doc_title)
    return okJsonResponse()


@app.route("/cite/<int:docid>/<path:newname>", methods=["GET"])
def create_citation_doc(docid, newname):
    params = request.get_json()

    # Filter for allowed reference parameters
    if params is not None:
        params = {k: params[k] for k in params if k in ('r', 'r_docid')}
        params['r'] = 'c'
    else:
        params = {'r': 'c'}

    timdb = getTimDb()
    if not has_view_access(docid):
        abort(403)

    src_doc = Document(docid)

    def factory(name, group):
        return timdb.documents.create_translation(src_doc, name, group, params).doc_id
    return create_item(newname, 'document', factory, getCurrentUserGroup())


@app.route("/createFolder", methods=["POST"])
def create_folder():
    jsondata = request.get_json()
    folder_name = jsondata['name']
    owner_group_name = jsondata['owner']
    timdb = getTimDb()
    owner_group_id = timdb.users.get_usergroup_by_name(owner_group_name)
    if owner_group_id is None:
        abort(404, 'Non-existent usergroup.')
    return create_item(folder_name, 'folder', timdb.folders.create, owner_group_id)


@app.route("/getBlock/<int:doc_id>/<par_id>")
def get_block(doc_id, par_id):
    verify_edit_access(doc_id)
    area_start = request.args.get('area_start')
    area_end = request.args.get('area_end')
    if area_start and area_end:
        return jsonResponse({"text": Document(doc_id).export_section(area_start, area_end)})
    else:
        par = Document(doc_id).get_paragraph(par_id)
        return jsonResponse({"text": par.get_exported_markdown()})


@app.route("/<plugin>/<path:filename>")
def plugin_call(plugin, filename):
    try:
        req = containerLink.call_plugin_resource(plugin, filename, request.args)
        return Response(stream_with_context(req.iter_content()), content_type=req.headers['content-type'])
    except PluginException:
        abort(404)


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
        return jsonResponse({'empty': True})
    else:
        return render_template('content.html',
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
        return jsonResponse(session['settings'])
    except (NameError, KeyError):
        abort(404)


@app.route("/getServerTime", methods=['GET'])
def get_server_time():
    t2 = int(time.time() * 1000)
    t1 = int(request.args.get('t1'))
    return jsonResponse({'t1': t1, 't2': t2, 't3': int(time.time() * 1000)})


@app.route("/")
def start_page():
    in_lecture = user_in_lecture()
    return render_template('start.html',
                           in_lecture=in_lecture)


@app.route("/view/")
def index_page():
    save_last_page()
    timdb = getTimDb()
    current_user = getCurrentUserId()
    in_lecture = user_in_lecture()
    possible_groups = timdb.users.get_usergroups_printable(current_user)
    return render_template('index.html',
                           userName=getCurrentUserName(),
                           userId=current_user,
                           userGroups=possible_groups,
                           in_lecture=in_lecture,
                           doc={'id': -1, 'fullname': ''},
                           rights={})


@app.route("/manage/")
@app.route("/slide/")
@app.route("/teacher/")
@app.route("/answers/")
@app.route("/lecture/")
def index_redirect():
    return redirect('/view')


@app.route("/getslidestatus/")
def getslidestatus():
    # TODO: Fix tempdb, this is a temporary aid
    return jsonResponse(None)

    if 'doc_id' not in request.args:
        abort(404, "Missing doc id")
    doc_id = int(request.args['doc_id'])
    tempdb = getTempDb()
    status = tempdb.slidestatuses.get_status(doc_id)
    if status:
        status = status.status
    else:
        status = None
    return jsonResponse(status)


@app.route("/setslidestatus")
def setslidestatus():
    if 'doc_id' not in request.args or 'status' not in request.args:
        abort(404, "Missing doc id or status")
    doc_id = int(request.args['doc_id'])
    verify_manage_access(doc_id)
    status = request.args['status']
    tempdb = getTempDb()
    tempdb.slidestatuses.update_or_add_status(doc_id, status)
    return jsonResponse("")


@app.before_request
def make_session_permanent():
    session.permanent = True


@app.after_request
def close_db(response):
    if hasattr(g, 'timdb'):
        g.timdb.close()
    return response


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
