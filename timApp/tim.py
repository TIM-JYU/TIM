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
from timApp.documentmodel.create_item import do_create_item, get_templates_for_folder, create_item
from timApp.documentmodel.document import Document
from timApp.documentmodel.documentversion import DocumentVersion
from timApp.logger import log_info, log_error, log_debug, log_warning
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
from timApp.timdb.documents import create_citation, create_translation
from timApp.timdb.exceptions import TimDbException, ItemAlreadyExistsException
from timApp.timdb.item import Item
from timApp.timdb.models.block import copy_default_rights
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.translation import Translation
from timApp.timdb.models.user import User
from timApp.timdb.tim_models import db
from timApp.timdb.userutils import NoSuchUserException

import timApp.tools.pdftools  # for pdf stamp & merge testing
from uuid import uuid4  # for pdf stamp & merge testing

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


# testing route for pdftools.py
@app.route('/pdftest')
def test_pdf():
    pdftestdata = [
        {'file': "static/testpdf/wlan.pdf",
         'date': "9.3.2018", 'attachment': "A", 'issue': "3"},
        {'file': "static/testpdf/TIM-esittely.pdf",
         'text': "LEIMATEKSTIN\n\nvoi valita\n\ntäysin vapaasti!"}
    ]
    output_name = f"static/testpdf/{uuid4()}.pdf"
    try:
        timApp.tools.pdftools.stamp_merge_pdfs(pdftestdata, output_name)
    except Exception as e:
        message = repr(e)
        abort(404, message)
    else:
        return send_file(output_name, mimetype="application/pdf")


# testing route for pdftools.py
@app.route('/pdfstamptest')
def test_pdf_stamp():
    pdftestdata = [
        {'file': "static/testpdf/wlan.pdf",
         'date': "9.3.2018", 'attachment': "A", 'issue': "3"},
        {'file': "static/testpdf/TIM-esittely.pdf",
         'text': "LEIMATEKSTIN\n\nvoi valita\n\ntäysin vapaasti!"}
    ]
    try:
        paths = timApp.tools.pdftools.stamp_merge_pdfs(pdftestdata, "", merge=False)
    except Exception as e:
        message = repr(e)
        abort(404, message)
    else:
        abort(404, repr(paths))


# testing route for pdftools.py
@app.route('/pdfmergetest')
def test_pdfmerge():
    output_name = f"static/testpdf/{uuid4()}.pdf"
    files = ['static/testpdf/13dae5e4-8608-4f2d-b25e-85ad64d758dd_1_stamped.pdf',
            'static/testpdf/13dae5e4-8608-4f2d-b25e-85ad64d758dd_2_stamped.pdf',
            'static/testpdf/13dae5e4-8608-4f2d-b25e-85ad64d758dd_3_stamped.pdf',
            'static/testpdf/13dae5e4-8608-4f2d-b25e-85ad64d758dd_4_stamped.pdf',
            'static/testpdf/13dae5e4-8608-4f2d-b25e-85ad64d758dd_5_stamped.pdf',
            'static/testpdf/13dae5e4-8608-4f2d-b25e-85ad64d758dd_6_stamped.pdf']
    try:
        timApp.tools.pdftools.merge_pdf(files, output_name)
    except Exception as e:
        message = repr(e)
        abort(404, message)
    else:
        return send_file(output_name, mimetype="application/pdf")


# testing route for processing pdf attachments
@app.route('/processAttachments/<path:doc>')
def process_attachments(doc):
    try:
        d = DocEntry.find_by_path(doc, try_translation=True)
        if not d:
            abort(404)
        verify_view_access(d)

        paragraphs = d.document.get_paragraphs(d)
        pdf_stamp_data = []
        for par in paragraphs:
            if par.is_plugin() and par.get_attr('plugin') == 'showPdf':
                plugin_creature = timApp.plugin.Plugin.from_paragraph(par)
                par_data = plugin_creature.values
                par_file = par_data["file"]
                # if attachment is url link, download it to temp folder and operate the
                # downloaded file
                if timApp.tools.pdftools.is_url(par_file):
                    print(f"Downloading {par_file}")
                    par_data["file"] = timApp.tools.pdftools.download_file_from_url(par_file)

                pdf_stamp_data += [par_data]
                print(repr(par_data))

        output_name = f"static/testpdf/{uuid4()}.pdf"
        # TODO: can't find files uploaded to TIM
        # TODO: raises FileNotFoundError if an error occurs inside try-finally block in pdftools?
        timApp.tools.pdftools.stamp_merge_pdfs(pdf_stamp_data, output_name)
    except Exception as e:
        message = repr(e)
        abort(404, message)
    else:
        return send_file(output_name, mimetype="application/pdf")


# route for creating extracts of faculty council minutes
@app.route('/createMinuteExtracts/<path:doc>')
def create_minute_extracts(doc):
    d = DocEntry.find_by_path(doc, try_translation=True)
    if not d:
        abort(404)
    verify_manage_access(d)

    # figure out the index of the minute, get the value of the 'nr' macro

    macros = d.document.get_settings().get_macroinfo().get_macros()
    if "nr" not in macros:
        return abort(Response("Error creating extracts: the document is not a minute document (no 'nr' macro found)"))

    minute_number = macros["nr"]
    if not isinstance(minute_number, int):
        return abort(Response("Error creating extracts: the value of the 'nr' macro is not a valid integer"))

    paragraphs = d.document.get_paragraphs(d)

    extract_dict = dict()
    current_paragraphs = []
    current_extract_index = -1

    # we build a dict of the extracts first before creating any new files
    # for each extract, we need its list index / extract index and the related paragraphs

    # we detect an extract by searching the document's non-expanded markdown for the extract macro
    # all paragraphs between two such macros belong to the same extract
    # the rest of the document after the last occurence of the macro belong to the last extract

    markdown_to_find = "%%lista("

    for par in paragraphs:
        markdown = par.get_markdown()
        macro_position = markdown.find(markdown_to_find)

        if macro_position > -1:
            # if the extract macro exists in the current paragraph, figure out the extract index from the markdown
            # if we can't parse the extract index, abort
            comma_position = markdown.find(",", macro_position + len(markdown_to_find))
            if comma_position == -1:
                abort(Response("Failed to parse extract index from macro, from paragraph: \n" + markdown))

            new_extract_index = 0
            try:
                new_extract_index = int(markdown[macro_position + 8:comma_position])
            except ValueError:
                abort(Response("Failed to parse extract index from macro, from paragraph: \n" + markdown))

            if current_extract_index > -1:
                # if we were in another extract's paragraph before, save the previous extract's paragraphs into the dict
                # don't allow duplicate extract numbers
                if current_extract_index in extract_dict:
                    return abort(Response("Error creating extracts: the same extract \
                     entry cannot exist multiple times in the document."))
                extract_dict[current_extract_index] = current_paragraphs
                # TODO check if assignment in python lists works "by reference" or whether they're copied
                # like c# structs
                # in other words, figure out if the copying here is necessary
                current_paragraphs = current_paragraphs.copy()
                current_paragraphs.clear()

            current_extract_index = new_extract_index
            current_paragraphs.append(par)

        elif current_extract_index > -1:
            # if the macro doesn't exist in the current paragraph but it existed in some previous paragraph,
            # we're in a paragraph related to an extract -> add the paragraph to the extract's list of paragraphs
            current_paragraphs.append(par)

    # after the loop has ended, check if we're still within an extract
    # if so, add the last extract to the dict
    if current_extract_index > -1:
        if current_extract_index in extract_dict:
            return abort(Response("Error creating extracts: the same extract entry cannot \
             exist multiple times in the document."))
        extract_dict[current_extract_index] = current_paragraphs

    if len(extract_dict) == 0:
        return abort(Response("The document has no extract macros!"))

    base_path = d.location + "/otteet/kokous" + str(minute_number) + "/"

    # create the composite document that has links to all the extract documents
    composite_docentry = do_create_item(base_path + "kokous" + str(minute_number), "document", "kokous"
                                        + str(minute_number), None, None)
    composite_docentry.document.add_paragraph("## Pöytäkirjan asiakohtien otteet")

    composite_paragraph = composite_docentry.document.add_paragraph("")

    # loop through the extracts and create new documents for them
    for extract_number, paragraphs in extract_dict.items():
        docentry = do_create_item(base_path + "lista" + str(extract_number), "document",
                                  "lista" + str(extract_number), None, None)
        for par in paragraphs:
            docentry.document.add_paragraph_obj(par.create_reference(docentry.document, add_rd=True))
        docentry.document.add_paragraph("Allekirjoitukset: _________________________")

        # add into the composite document a link leading to the new extract document
        composite_paragraph.set_markdown(composite_paragraph.get_markdown() + "\n" + "- [Lista " + str(extract_number) +
                                         "](lista" + str(extract_number) + "), ([PDF](/print/" +
                                         docentry.path_without_lang + "))")

    composite_paragraph.save()
    db.session.commit()
    return safe_redirect("/view/" + composite_docentry.path_without_lang)


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
    cite_id, copy_id, template_name = verify_json_params('cite', 'copy', 'template', require=False)
    if cite_id:
        return create_citation_doc(cite_id, item_path, item_title)

    d = None
    if copy_id:
        d = get_doc_or_abort(copy_id)
        verify_edit_access(d)
        d = d.src_doc
        vr = d.document.validate()
        if vr.issues:
            abort(400, f'The following errors must be fixed before copying:\n{vr}')
    item = do_create_item(item_path, item_type, item_title, d, template_name)
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


def create_citation_doc(doc_id, doc_path, doc_title):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)

    src_doc = d.document

    def factory(path, group, title):
        return create_citation(src_doc, group, path, title)

    item = create_item(doc_path, 'document', doc_title, factory, get_current_user_group())
    db.session.commit()
    return json_response(item)


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


@app.after_request
def log_request(response):
    if not request.path.startswith('/static/') and request.path != '/favicon.ico':
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
