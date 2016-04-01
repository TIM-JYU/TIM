"""Routes for editing a document."""
from bs4 import UnicodeDammit
from flask import Blueprint, render_template

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from documentmodel.documentparser import DocumentParser, ValidationException, ValidationWarning
from documentmodel.documentparseroptions import DocumentParserOptions
from markdownconverter import md_to_html
from routes import logger
from routes.notify import notify_doc_owner
from timdb.timdbbase import TimDbException
from utils import get_error_html
from .common import *

edit_page = Blueprint('edit_page',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@edit_page.route('/update/<int:doc_id>', methods=['POST'])
def update_document(doc_id):
    """Route for updating a document as a whole.

    :param doc_id: The id of the document to be modified.
    :return: A JSON object containing the versions of the document.
    """
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id):
        abort(404)
    if not timdb.users.has_edit_access(getCurrentUserId(), doc_id):
        abort(403)
    if 'file' in request.files:
        doc = request.files['file']
        raw = doc.read()

        # UnicodeDammit gives incorrect results if the encoding is UTF-8 without BOM,
        # so try the built-in function first.
        try:
            content = raw.decode('utf-8')
        except UnicodeDecodeError:
            content = UnicodeDammit(raw).unicode_markup
        original = request.form['original']
        strict_validation = not request.form.get('ignore_warnings', False)
    elif 'template_name' in request.get_json():
        template_id = timdb.documents.get_document_id(request.get_json()['template_name'])
        if not has_manage_access(template_id):
            abort(403, 'Permission denied')
        doc = Document(template_id)
        content = doc.export_markdown()
        if content == '':
            return abort(400, 'The selected template is empty.')
        original = ''
        strict_validation = True
    else:
        request_json = request.get_json()
        if 'fulltext' not in request_json:
            return abort(400, 'Malformed request - fulltext missing.')
        content = request_json['fulltext']
        original = request_json['original']
        strict_validation = not request_json.get('ignore_warnings', False)

    if original is None:
        abort(400, 'Missing parameter: original')
    if content is None:
        return jsonResponse({'message': 'Failed to convert the file to UTF-8.'}, 400)
    doc = get_newest_document(doc_id)
    try:
        # To verify view rights for possible referenced paragraphs, we call this first:
        get_pars_from_editor_text(doc, content, break_on_elements=True)
        d = timdb.documents.update_document(doc, content, original, strict_validation)
    except ValidationWarning as e:
        return jsonResponse({'error': str(e), 'is_warning': True}, status_code=400)
    except (TimDbException, ValidationException) as e:
        return abort(400, str(e))
    chg = d.get_changelog()
    for ver in chg:
        ver['group'] = timdb.users.get_user_group_name(ver.pop('group_id'))

    # todo: include diffs in the message
    notify_doc_owner(doc_id, '[user_name] has edited your document [doc_name]',
                     '[user_name] has edited your document as whole: [doc_url]', setting="doc_modify")

    return jsonResponse({'versions': chg, 'fulltext': d.export_markdown()})


@edit_page.route("/postParagraph/", methods=['POST'])
def modify_paragraph():
    """
    Route for modifying a paragraph in a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    timdb = getTimDb()
    doc_id, md, par_id, par_next_id = verify_json_params('docId', 'text', 'par', 'par_next')
    verify_edit_access(doc_id)

    logger.log_message("Editing file: {}, paragraph {}".format(doc_id, par_id), 'INFO')
    doc = get_newest_document(doc_id)
    if not doc.has_paragraph(par_id):
        abort(400, 'Paragraph not found: ' + par_id)

    area_start = request.get_json().get('area_start')
    area_end = request.get_json().get('area_end')
    editing_area = area_start and area_end
    try:
        editor_pars = get_pars_from_editor_text(doc, md, break_on_elements=editing_area)
    except ValidationException as e:
        return abort(400, str(e))

    if editing_area:
        try:
            original_md = doc.export_section(area_start, area_end)
            new_start, new_end = doc.update_section(md, area_start, area_end)
            updated_md = doc.export_section(new_start, new_end)
            pars = doc.get_section(new_start, new_end)
        except (ValidationException, TimDbException) as e:
            return abort(400, str(e))
    else:
        original_par = doc.get_paragraph(par_id)
        original_md = doc.export_section(par_id, par_id)
        pars = []

        separate_pars = DocumentParser(md).get_blocks()
        is_multi_block = len(separate_pars) > 1
        has_headers = None
        if is_multi_block:
            for separate_par in separate_pars:
                if separate_par['type'] == 'header':
                    has_headers = True
                    break

        if editor_pars[0].is_different_from(original_par):
            properties = {}
            if is_multi_block:
                properties['multi_block'] = True
            if has_headers:
                properties['has_headers'] = has_headers
            [par], _ = timdb.documents.modify_paragraph(doc=doc,
                                                        par_id=par_id,
                                                        new_content=editor_pars[0].get_markdown(),
                                                        new_attrs=editor_pars[0].get_attrs(),
                                                        new_properties=properties)
            pars.append(par)
        else:
            # If the first paragraph was not modified at all, append the original one
            pars.append(original_par)
        for p in editor_pars[1:]:
            properties = {}
            if is_multi_block:
                properties['multi_block'] = True
            if has_headers:
                properties['has_headers'] = has_headers
            [par], _ = timdb.documents.add_paragraph(doc, p.get_markdown(), par_next_id, attrs=p.get_attrs(),
                                                     properties=properties)
            pars.append(par)

        updated_md = doc.export_section(pars[0].get_id(), pars[len(pars) - 1].get_id())

    mark_pars_as_read_if_chosen(pars, doc)

    notify_doc_owner(doc_id,
                     '[user_name] has edited your document [doc_name]',
                     """[user_name] has changed a paragraph in your document [doc_url]\n
== ORIGINAL ==\n
{}\n\n
==MODIFIED==\n
{}\n
""".format(original_md, updated_md), setting="doc_modify", par_id=par_id)

    return par_response(pars,
                        doc,
                        update_cache=current_app.config['IMMEDIATE_PRELOAD'])


@edit_page.route("/preview/<int:doc_id>", methods=['POST'])
def preview(doc_id):
    """Route for previewing a paragraph.

    :param doc_id: The id of the document in which the preview will be rendered.
    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    text, = verify_json_params('text')
    rjson = request.get_json()
    if not rjson.get('isComment'):
        area_start = rjson.get('area_start')
        editing_area = area_start is not None and rjson.get('area_end') is not None
        doc = Document(doc_id)
        next_par_id = rjson.get('par_next')
        if editing_area:
            context_par = doc.get_previous_par(doc.get_paragraph(area_start))
        elif next_par_id:
            context_par = doc.get_previous_par(doc.get_paragraph(next_par_id))
            if doc.has_paragraph(rjson.get('par')):  # rjson['par'] is 'NEW_PAR' when adding a new paragraph
                context_par = doc.get_previous_par(context_par)
        else:
            context_par = doc.get_last_par()
        try:
            blocks = get_pars_from_editor_text(doc, text, break_on_elements=editing_area)
            doc.insert_temporary_pars(blocks, context_par)
            return par_response(blocks, doc, edit_window=True, context_par=context_par)
        except Exception as e:
            err_html = get_error_html(e)
            blocks = [DocParagraph.create(doc=doc, md='', html=err_html)]
            return par_response(blocks, doc, edit_window=True)
    else:
        return jsonResponse({'texts': md_to_html(text), 'js': [], 'css': []})


def par_response(blocks, doc, edit_window=False, update_cache=False, context_par=None):
    if update_cache:
        changed_pars = DocParagraph.preload_htmls(doc.get_paragraphs(),
                                                  doc.get_settings(),
                                                  persist=update_cache)
    else:
        changed_pars = []
        DocParagraph.preload_htmls(blocks, doc.get_settings(), context_par=context_par, persist=update_cache)

    current_user = get_current_user()
    pars, js_paths, css_paths, modules = post_process_pars(doc, blocks, current_user, edit_window=edit_window)

    changed_pars, _, _, _ = post_process_pars(doc, changed_pars, current_user, edit_window=edit_window)

    return jsonResponse({'texts': render_template('paragraphs.html',
                                                  text=pars,
                                                  rights=get_rights(doc.doc_id),
                                                  preview=edit_window),
                         'route': 'preview',
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules,
                         'changed_pars': {p['id']: render_template('paragraphs.html',
                                                                   text=[p],
                                                                   rights=get_rights(doc.doc_id)) for p in
                                          changed_pars},
                         'version': doc.get_version()})


def get_pars_from_editor_text(doc, text, break_on_elements=False):
    options = DocumentParserOptions()
    options.break_on_code_block = break_on_elements
    options.break_on_header = break_on_elements
    options.break_on_normal = break_on_elements
    blocks = [DocParagraph.create(doc=doc, md=par['md'], attrs=par.get('attrs'))
              for par in DocumentParser(text).validate_structure(
                  is_whole_document=False).get_blocks(options)]
    for p in blocks:
        if p.is_reference():
            try:
                refdoc = int(p.get_attr('rd'))
            except (ValueError, TypeError):
                continue
            if getTimDb().documents.exists(refdoc)\
                    and not getTimDb().users.has_view_access(getCurrentUserId(), refdoc):
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
    verify_edit_access(doc_id)
    doc = get_newest_document(doc_id)
    try:
        editor_pars = get_pars_from_editor_text(doc, md)
    except ValidationException as e:
        return abort(400, str(e))

    pars = []
    separate_pars = DocumentParser(md).get_blocks()
    is_multi_block = len(separate_pars) > 1
    has_headers = None
    if is_multi_block:
        for separate_par in separate_pars:
            if separate_par['type'] == 'header':
                has_headers = True
                break
    for p in editor_pars:
        properties = {}
        if is_multi_block:
            properties['multi_block'] = True
        if has_headers:
            properties['has_headers'] = has_headers
        [par], _ = timdb.documents.add_paragraph(doc, p.get_markdown(), par_next_id, attrs=p.get_attrs(),
                                                 properties=properties)
        pars.append(par)
    mark_pars_as_read_if_chosen(pars, doc)

    notify_doc_owner(doc_id,
                     '[user_name] has edited your document [doc_name]',
                     '[user_name] has added a new paragraph on your document [doc_url]\n\n{}'.format(md),
                     setting="doc_modify", par_id=pars[0].get_id() if len(pars) > 0 else None)

    return par_response(pars, doc, update_cache=current_app.config['IMMEDIATE_PRELOAD'])


@edit_page.route("/deleteParagraph/<int:doc_id>", methods=["POST"])
def delete_paragraph(doc_id):
    """Route for deleting a paragraph from a document.

    :param doc_id: The id of the document.
    :return: A JSON object containing the version of the new document.
    """
    timdb = getTimDb()
    verify_edit_access(doc_id)
    area_start, area_end = verify_json_params('area_start', 'area_end', require=False)
    doc = get_newest_document(doc_id)
    if area_end and area_start:
        text = doc.export_section(area_start, area_end)
        doc.delete_section(area_start, area_end)
    else:
        par_id, = verify_json_params('par')
        text = doc.export_section(par_id, par_id)
        timdb.documents.delete_paragraph(doc, par_id)

    user_name = getCurrentUserName()
    doc_name = timdb.documents.get_first_document_name(doc_id)
    notify_doc_owner(doc_id, '[user_name] has edited your document [doc_name]',
                     '[user_name] has deleted the following paragraph(s) from your document [doc_url]\n\n{}'.format(
                         text), setting="doc_modify")

    return par_response([], doc, update_cache=current_app.config['IMMEDIATE_PRELOAD'])


@edit_page.route("/getUpdatedPars/<int:doc_id>")
def get_updated_pars(doc_id):
    """
    Gets updated paragraphs that were changed e.g. as the result of adding headings or modifying macros.
    :param doc_id: The document id.
    """
    verify_view_access(doc_id)
    return par_response([], Document(doc_id), update_cache=True)
