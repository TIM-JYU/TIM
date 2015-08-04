"""Routes for editing a document."""
from bs4 import UnicodeDammit
from flask import Blueprint

from .common import *
from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from documentmodel.documentparser import DocumentParser, ValidationException
import pluginControl
from timdb.docidentifier import DocIdentifier
from timdb.timdbbase import TimDbException

edit_page = Blueprint('edit_page',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@edit_page.route('/update/<int:doc_id>/<version>', methods=['POST'])
def update_document(doc_id, version):
    """Route for updating a document as a whole.

    :param doc_id: The id of the document to be modified.
    :param version: The version string of the current document version.
    :return: A JSON object containing the versions of the document.
    """
    timdb = getTimDb()
    doc_identifier = DocIdentifier(doc_id, version)
    if not timdb.documents.exists(doc_id):
        abort(404)
    if not timdb.users.userHasEditAccess(getCurrentUserId(), doc_id):
        abort(403)
    # verify_document_version(doc_id, version)
    if 'file' in request.files:
        doc = request.files['file']
        raw = doc.read()

        # UnicodeDammit gives incorrect results if the encoding is UTF-8 without BOM,
        # so try the built-in function first.
        try:
            content = raw.decode('utf-8')
        except UnicodeDecodeError:
            content = UnicodeDammit(raw).unicode_markup
    else:
        request_json = request.get_json()
        if 'fulltext' not in request_json:
            return jsonResponse({'message': 'Malformed request - fulltext missing.'}, 400)
        content = request_json['fulltext']

    if content is None:
        return jsonResponse({'message': 'Failed to convert the file to UTF-8.'}, 400)
    doc = Document(doc_id, modifier_group_id=getCurrentUserGroup())
    try:
        d = timdb.documents.update_document(doc, content)
    except TimDbException as e:
        abort(400, str(e))
        return
    chg = d.get_changelog()
    for ver in chg:
        ver['group'] = timdb.users.get_user_group_name(ver.pop('group_id'))
    return jsonResponse({'versions': chg, 'fulltext': d.export_markdown()})


@edit_page.route("/postParagraph/", methods=['POST'])
def modify_paragraph():
    """
    Route for modifying a paragraph in a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    timdb = getTimDb()
    doc_id, md, par_id, par_next_id, attrs = verify_json_params('docId', 'text', 'par', 'par_next', 'attrs')
    verifyEditAccess(doc_id)

    current_app.logger.info("Editing file: {}, paragraph {}".format(doc_id, par_id))
    version = request.headers.get('Version', '')
    # verify_document_version(doc_id, version)
    doc = get_newest_document(doc_id)
    if not doc.has_paragraph(par_id):
        abort(400, 'Paragraph not found: ' + par_id)

    area_start = request.get_json().get('area_start')
    area_end = request.get_json().get('area_end')
    editing_area = area_start and area_end
    editor_pars = get_pars_from_editor_text(doc_id, md, break_on_elements=editing_area)

    if editing_area:
        try:
            new_start, new_end = doc.update_section(md, area_start, area_end)
            pars = doc.get_section(new_start, new_end)
        except ValidationException as e:
            return abort(400, str(e))
    else:
        original_par = DocParagraph(doc_id=doc_id, par_id=par_id)
        pars = []
        if editor_pars[0].is_different_from(original_par):
            [par], _ = timdb.documents.modify_paragraph(doc,
                                                        par_id,
                                                        editor_pars[0].get_markdown(),
                                                        editor_pars[0].get_attrs())
            pars.append(par)

        for p in editor_pars[1:]:
            [par], _ = timdb.documents.add_paragraph(doc, p.get_markdown(), par_next_id, attrs=p.get_attrs())
            pars.append(par)
    mark_pars_as_read_if_chosen(pars, doc)
    pars, js_paths, css_paths, modules, read_statuses = post_process_pars(pars, doc_id)
    return jsonResponse({'texts': pars,
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules,
                         'version': doc.get_version(),
                         'read_statuses': read_statuses})


@edit_page.route("/preview/<int:doc_id>", methods=['POST'])
def preview(doc_id):
    """Route for previewing a paragraph.

    :param doc_id: The id of the document in which the preview will be renderer. Unused so far.
    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    timdb = getTimDb()
    text, = verify_json_params('text')
    editing_area = request.get_json().get('area_start') is not None and request.get_json().get('area_end') is not None
    blocks = get_pars_from_editor_text(doc_id, text, break_on_elements=editing_area)
    pars, js_paths, css_paths, modules, _ = post_process_pars(blocks, doc_id)
    return jsonResponse({'texts': pars,
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules})


def get_pars_from_editor_text(doc_id, text, break_on_elements=False):
    blocks = [DocParagraph(doc_id=doc_id, md=par['md'], attrs=par.get('attrs'))
              for par in DocumentParser(text).get_blocks(break_on_code_block=break_on_elements,
                                                         break_on_header=break_on_elements,
                                                         break_on_normal=break_on_elements)]
    return blocks


def mark_pars_as_read_if_chosen(pars, doc):
    """Marks the specified paragraphs as read if tags.markread is true in request's JSON data.

    :type doc: Document
    :type pars: list[DocParagraph]
    :param pars: The paragraphs to be marked as read
    :param doc: The document to which the paragraphs belong.
    """
    mark_read = request.get_json().get('tags', {}).get('markread')
    timdb = getTimDb()
    if mark_read:
        for p in pars:
            timdb.readings.setAsRead(getCurrentUserGroup(), doc, p)


@edit_page.route("/newParagraph/", methods=["POST"])
def add_paragraph():
    """Route for adding a new paragraph to a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    timdb = getTimDb()
    md, doc_id, par_next_id = verify_json_params('text', 'docId', 'par_next')
    verifyEditAccess(doc_id)
    version = request.headers.get('Version', '')
    editor_pars = get_pars_from_editor_text(doc_id, md)

    # verify_document_version(doc_id, version)
    doc = get_newest_document(doc_id)
    pars = []
    for p in editor_pars:
        [par], _ = timdb.documents.add_paragraph(doc, p.get_markdown(), par_next_id, attrs=p.get_attrs())
        pars.append(par)
    mark_pars_as_read_if_chosen(pars, doc)
    pars, js_paths, css_paths, modules, read_statuses = post_process_pars(pars, doc_id)
    return jsonResponse({'texts': pars,
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules,
                         'version': doc.get_version(),
                         'read_statuses': read_statuses})


@edit_page.route("/deleteParagraph/<int:doc_id>", methods=["POST"])
def delete_paragraph(doc_id):
    """Route for deleting a paragraph from a document.

    :param doc_id: The id of the document.
    :return: A JSON object containing the version of the new document.
    """
    timdb = getTimDb()
    verifyEditAccess(doc_id)
    version = request.headers.get('Version', '')
    area_start, area_end = verify_json_params('area_start', 'area_end', require=False)
    # verify_document_version(doc_id, version)
    if area_end and area_start:
        new_doc = Document(doc_id)
        new_doc.delete_section(area_start, area_end)
    else:
        par_id, = verify_json_params('par')
        new_doc = timdb.documents.delete_paragraph(get_newest_document(doc_id), par_id)
    return jsonResponse({'version': new_doc.get_version()})


def post_process_pars(pars, doc_id):
    timdb = getTimDb()
    pars, js_paths, css_paths, modules = pluginControl.pluginify(pars,
                                                                 getCurrentUserName(),
                                                                 timdb.answers,
                                                                 doc_id,
                                                                 getCurrentUserId())
    readings = timdb.readings.getReadings(getCurrentUserGroup(), Document(doc_id))
    pars_dict = dict((par.get_id(), par) for par in pars)
    read_statuses = {}
    for r in readings:
        if r['par_id'] in pars_dict:
            read_statuses[r['par_id']] = 'read' if r['par_hash'] == pars_dict[r['par_id']].get_hash() else 'modified'
    return pars, js_paths, css_paths, modules, read_statuses
