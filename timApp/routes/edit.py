"""Routes for editing a document."""
from bs4 import UnicodeDammit
from flask import Blueprint, render_template

from .common import *
from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from documentmodel.documentparser import DocumentParser, ValidationException
from htmlSanitize import sanitize_html
from routes.common import post_process_pars
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
        # To verify view rights for possible referenced paragraphs, we call this first:
        get_pars_from_editor_text(doc_id, content, break_on_elements=True)
        d = timdb.documents.update_document(doc, content)
    except (TimDbException, ValidationException) as e:
        return abort(400, str(e))
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
    try:
        editor_pars = get_pars_from_editor_text(doc_id, md, break_on_elements=editing_area)
    except ValidationException as e:
        return abort(400, str(e))

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
    return par_response(pars, doc_id)


@edit_page.route("/preview/<int:doc_id>", methods=['POST'])
def preview(doc_id):
    """Route for previewing a paragraph.

    :param doc_id: The id of the document in which the preview will be renderer. Unused so far.
    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    text, = verify_json_params('text')
    editing_area = request.get_json().get('area_start') is not None and request.get_json().get('area_end') is not None
    try:
        blocks = get_pars_from_editor_text(doc_id, text, break_on_elements=editing_area)
    except ValidationException as e:
        blocks = [DocParagraph(doc_id=doc_id)]
        blocks[0].set_html('<div class="pluginError">{}</div>'.format(sanitize_html(str(e))))
    return par_response(blocks, doc_id)


def par_response(blocks, doc_id):
    pars, js_paths, css_paths, modules, read_statuses, notes = post_process_pars(blocks, doc_id, getCurrentUserId())
    return jsonResponse({'texts': render_template('paragraphs.html',
                                                  text=pars,
                                                  readings=read_statuses,
                                                  notes=notes,
                                                  rights=get_rights(doc_id)),
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules,
                         'version': Document(doc_id).get_version()})


def get_pars_from_editor_text(doc_id, text, break_on_elements=False):
    blocks = [DocParagraph(doc_id=doc_id, md=par['md'], attrs=par.get('attrs'))
              for par in DocumentParser(text).validate_structure().get_blocks(break_on_code_block=break_on_elements,
                                                                              break_on_header=break_on_elements,
                                                                              break_on_normal=break_on_elements)]
    for p in blocks:
        if p.is_reference():
            try:
                refdoc = int(p.get_attrs().get('rd'))
            except ValueError:
                return blocks
            if getTimDb().documents.exists(refdoc)\
                    and not getTimDb().users.userHasViewAccess(getCurrentUserId(), refdoc):
                raise ValidationException("You don't have view access to document {}".format(refdoc))
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
    try:
        editor_pars = get_pars_from_editor_text(doc_id, md)
    except ValidationException as e:
        return abort(400, str(e))

    # verify_document_version(doc_id, version)
    doc = get_newest_document(doc_id)
    pars = []
    for p in editor_pars:
        [par], _ = timdb.documents.add_paragraph(doc, p.get_markdown(), par_next_id, attrs=p.get_attrs())
        pars.append(par)
    mark_pars_as_read_if_chosen(pars, doc)
    return par_response(pars, doc_id)


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
