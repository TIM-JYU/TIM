"""Routes for editing a document."""
from bs4 import UnicodeDammit
from flask import Blueprint, render_template

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from documentmodel.documentparser import DocumentParser, ValidationException, ValidationWarning
from documentmodel.documentparseroptions import DocumentParserOptions
from markdownconverter import md_to_html
from routes.logger import log_info
from routes.notify import notify_doc_owner
from timdb.bookmarks import Bookmarks
from timdb.models.docentry import DocEntry
from timdb.timdbexception import TimDbException
from typing import List
from utils import get_error_html
from .common import *
import yaml


edit_page = Blueprint('edit_page',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


def get_group_id() -> str:
    return "docmodify_[doc_id]"


def get_group_subject() -> str:
    return "Your document [doc_name] has been modified"


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
    ver_before = doc.get_version()
    try:
        # To verify view rights for possible referenced paragraphs, we call this first:
        editor_pars = get_pars_from_editor_text(doc, content, break_on_elements=True, skip_access_check=True)
        d = timdb.documents.update_document(doc, content, original, strict_validation)
        check_and_rename_pluginnamehere(editor_pars, d)
        old_pars = d.get_paragraphs()
        i = 0
        while i < len(editor_pars):
            if editor_pars[i].get_attr('taskId') and old_pars[i].get_attr('taskId'):
                if editor_pars[i].get_attr('taskId') != old_pars[i].get_attr('taskId'):
                    timdb.documents.modify_paragraph(doc=d,
                                                     par_id=old_pars[i].get_id(),
                                                     new_content=old_pars[i].get_markdown(),
                                                     new_attrs=editor_pars[i].get_attrs(),
                                                     new_properties=old_pars[i].get_properties())
            i += 1
        doc = get_newest_document(doc_id)
        duplicates = check_duplicates(doc.get_paragraphs(), doc, timdb)

    except ValidationWarning as e:
        return jsonResponse({'error': str(e), 'is_warning': True}, status_code=400)
    except (TimDbException, ValidationException) as e:
        return abort(400, str(e))
    chg = d.get_changelog()
    ver_after = d.get_version()
    for ver in chg:
        ver['group'] = timdb.users.get_user_group_name(ver.pop('group_id'))

    # todo: include diffs in the message
    notify_doc_owner(doc_id, '[user_name] has edited your document [doc_name]',
                     """[user_name] has edited your document as whole: [doc_url]\n\n
See the changes here:
[base_url]/diff/[doc_id]/{0}/{1}/{2}/{3}\n\n""".format(ver_before[0], ver_before[1], ver_after[0], ver_after[1]),
                     setting="doc_modify",
                     group_id=get_group_id(), group_subject=get_group_subject())

    return jsonResponse({'versions': chg, 'fulltext': d.export_markdown(), 'duplicates': duplicates})


@edit_page.route("/postNewTaskNames/", methods=['POST'])
def rename_task_ids():
    timdb = getTimDb()
    doc_id, duplicates = verify_json_params('docId', 'duplicates')
    manage_view = verify_json_params('manageView', require=False, default=False)
    verify_edit_access(doc_id)
    doc = get_newest_document(doc_id)
    pars = []
    old_pars = doc.get_paragraphs()

    # Get paragraphs with taskIds
    for paragraph in old_pars:
        if not paragraph.get_attr('taskId'):
            old_pars.remove(paragraph)
    i = 0
    while len(duplicates) > i:
        duplicate = duplicates[i]
        # Check that paragraphs that were to be modified aren't deleted
        if not doc.has_paragraph(duplicate[2]):
            duplicates.remove(duplicate)
            continue
        else:
            original_par = doc.get_paragraph(duplicate[2])
            attrs = original_par.get_attrs()
            if attrs['taskId']:
                # If a new taskId was given use that
                if duplicate[1]:
                    attrs['taskId'] = duplicate[1]
                    duplicates.remove(duplicate)
                # Otherwise determine a new one
                else:
                    if old_pars:
                        # Remove the duplicate in question from duplicates
                        duplicates.remove(duplicate)
                        task_id = get_next_available_task_id(attrs, old_pars, duplicates, duplicate[2])
                        attrs['taskId'] = task_id
            # Modify the paragraph with the new taskId
            [par], _ = timdb.documents.modify_paragraph(doc=doc,
                                                        par_id=duplicate[2],
                                                        new_content=original_par.get_markdown(),
                                                        new_attrs=attrs,
                                                        new_properties=original_par.get_properties())
            pars.append(par)
            # Update old pars
            for old_par in old_pars:
                if old_par.get_id() == duplicate[2]:
                    old_pars.remove(old_par)
                    break
            old_pars.append(par)

    if not manage_view:
        return par_response(pars,
                            doc,
                            update_cache=current_app.config['IMMEDIATE_PRELOAD'])
    else:
        doc = get_newest_document(doc_id)
        duplicates = check_duplicates(pars, doc, timdb)
        chg = doc.get_changelog()
        for ver in chg:
            ver['group'] = timdb.users.get_user_group_name(ver.pop('group_id'))

        # todo: include diffs in the message
        notify_doc_owner(doc_id, '[user_name] has edited your document [doc_name]',
                        '[user_name] has edited your document as whole: [doc_url]', setting="doc_modify",
                         group_id=get_group_id(), group_subject=get_group_subject())

        return jsonResponse({'versions': chg, 'fulltext': doc.export_markdown(), 'duplicates': duplicates})


def question_convert_js_to_yaml(md):
    mdjson = json.loads(md)
    question = mdjson["question"]
    del mdjson["question"]
    mdyaml = yaml.dump(mdjson) # spoils ä and so on..
    mdyaml = mdyaml.replace("\\xC4","Ä").replace("\\xD6","Ö").replace("\\xC5","Å").replace("\\xE4","ä").replace("\\xF6","ö").replace("\\xE5","å")
    return '``` {question="' + question + '"}\n'  + mdyaml + '```\n'


@edit_page.route("/postParagraphQ/", methods=['POST'])
def modify_paragraphQ():
    """
    Route for modifying a question editor paragraph in a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    doc_id, md, par_id, par_next_id = verify_json_params('docId', 'text', 'par', 'par_next')
    # abort(400, 'Not yet: ' + par_id)
    md = question_convert_js_to_yaml(md)
    ret = modify_paragraph_common(doc_id, md, par_id, par_next_id)
    # ret["questionjson"] = ""
    return ret


@edit_page.route("/postParagraph/", methods=['POST'])
def modify_paragraph():
    """
    Route for modifying a paragraph in a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    doc_id, md, par_id, par_next_id = verify_json_params('docId', 'text', 'par', 'par_next')
    return modify_paragraph_common(doc_id, md, par_id, par_next_id)


def modify_paragraph_common(doc_id, md, par_id, par_next_id):
    timdb = getTimDb()
    verify_edit_access(doc_id)

    log_info("Editing file: {}, paragraph {}".format(doc_id, par_id))
    doc = get_newest_document(doc_id)
    if not doc.has_paragraph(par_id):
        abort(400, 'Paragraph not found: ' + par_id)

    original_par = None
    version_before = doc.get_version()
    area_start = request.get_json().get('area_start')
    area_end = request.get_json().get('area_end')
    editing_area = area_start and area_end
    try:
        editor_pars = get_pars_from_editor_text(doc, md, break_on_elements=editing_area, skip_access_check=True)
    except ValidationException as e:
        return abort(400, str(e))

    editor_pars = check_and_rename_pluginnamehere(editor_pars, doc)
    new_par_ids = []

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
            new_par_ids = []
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
            new_par_ids.append(par.get_id())

        updated_md = doc.export_section(pars[0].get_id(), pars[len(pars) - 1].get_id())

    mark_pars_as_read_if_chosen(pars, doc)

    version_after = doc.get_version()
    notify_doc_owner(doc_id,
                     '[user_name] has edited your document [doc_name]',
                     """[user_name] has changed a paragraph in your document [doc_url]\n\n
See the changes here:
[base_url]/diff/[doc_id]/{2}/{3}/{4}/{5}\n\n
== ORIGINAL ==\n
{0}\n\n
==MODIFIED==\n
{1}\n
""".format(original_md, updated_md, version_before[0], version_before[1],
           version_after[0], version_after[1]), setting="doc_modify", par_id=par_id,
           group_id=get_group_id(), group_subject=get_group_subject())

    return par_response(pars,
                        doc,
                        original_par,
                        new_par_ids,
                        update_cache=current_app.config['IMMEDIATE_PRELOAD'],
                        edited=True)


@edit_page.route("/preview/<int:doc_id>", methods=['POST'])
def preview_paragraphs(doc_id):
    """Route for previewing paragraphs.

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
            return par_response(blocks, doc, preview=True, context_par=context_par)
        except Exception as e:
            err_html = get_error_html(e)
            blocks = [DocParagraph.create(doc=doc, md='', html=err_html)]
            return par_response(blocks, doc, preview=True)
    else:
        return jsonResponse({'texts': md_to_html(text), 'js': [], 'css': []})


def par_response(blocks,
                 doc,
                 original_par=None,
                 new_par_ids=None,
                 preview=False,
                 update_cache=False,
                 context_par=None,
                 edited=False):
    if update_cache:
        changed_pars = DocParagraph.preload_htmls(doc.get_paragraphs(),
                                                  doc.get_settings(),
                                                  persist=update_cache)
    else:
        changed_pars = []
        DocParagraph.preload_htmls(blocks, doc.get_settings(), context_par=context_par, persist=update_cache)

    # Do not check for duplicates for preview because the operation is heavy
    if not preview:
        duplicates = check_duplicates(blocks, doc, getTimDb())
        if edited and logged_in():
            bms = Bookmarks(get_current_user_object())
            d = DocEntry.find_by_id(doc.doc_id)
            bms.add_bookmark('Last edited', d.get_short_name(), '/view/' + d.get_path(), move_to_top=True).save_bookmarks()
    else:
        duplicates = None

    current_user = get_current_user()
    pars, js_paths, css_paths, modules = post_process_pars(doc, blocks, current_user, edit_window=preview,
                                                           show_questions=True)

    changed_pars, _, _, _ = post_process_pars(doc, changed_pars, current_user, edit_window=preview,
                                              show_questions=True)

    return jsonResponse({'texts': render_template('paragraphs.html',
                                                  text=pars,
                                                  rights=get_rights(doc.doc_id),
                                                  preview=preview),
                         'route': 'preview',
                         'js': js_paths,
                         'css': css_paths,
                         'angularModule': modules,
                         'changed_pars': {p['id']: render_template('paragraphs.html',
                                                                   text=[p],
                                                                   rights=get_rights(doc.doc_id)) for p in
                                          changed_pars},
                         'version': doc.get_version(),
                         'duplicates': duplicates,
                         'original_par': original_par,
                         'new_par_ids': new_par_ids
                         })


def get_pars_from_editor_text(doc: Document, text: str,
                              break_on_elements: bool = False, skip_access_check: bool = False) -> List[DocParagraph]:

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
            if not skip_access_check and getTimDb().documents.exists(refdoc)\
                    and not getTimDb().users.has_view_access(getCurrentUserId(), refdoc):
                raise ValidationException("You don't have view access to document {}".format(refdoc))
    return blocks


# Gets next available name for plugin
def get_next_available_task_id(attrs, old_pars, duplicates, par_id):
    task_id = attrs['taskId']
    need_new_task_id = False
    # First try with original name
    i = 0
    while i < len(old_pars):
        if old_pars[i].get_attr('taskId') == task_id:
            old_par_id = old_pars[i].get_id()
            # Ignore pars that are to be renamed
            if old_par_id == par_id:
                i += 1
                continue
            else:
                for par in duplicates:
                    if old_par_id == par[2]:
                        # Flip this bool value for convenience
                        need_new_task_id = not need_new_task_id
                        break
                need_new_task_id = not need_new_task_id
                if need_new_task_id:
                    break
                else:
                    i += 1
                    continue
        else:
            i += 1

    # If there was no previous par with the same task id keep it
    if not need_new_task_id:
        return task_id

    # Otherwise determine a new one
    else:
        # Split the name into text and trailing number
        task_id_body = ""
        task_id_number = None

        i = len(task_id) - 1
        while i >= 0:
            if task_id[i] in "0123456789":
                i -= 1
            else:
                text_part = task_id[:i+1]
                task_id_body = text_part
                if i < len(task_id) - 1:
                    task_id_number = int(task_id[(i+1):])
                break
        if not task_id_body:
            task_id_body = task_id
        if task_id_number is not None:
            task_id = task_id_body + str(task_id_number)
        else:
            task_id_number = 1
        j = 0
        while j < len(old_pars):
            if old_pars[j].get_attr('taskId') == task_id:
                task_id_number += 1
                task_id = task_id_body + str(task_id_number)
                j = 0
            else:
                j += 1
        return task_id


# Automatically rename plugins with name pluginnamehere
def check_and_rename_pluginnamehere(blocks, doc):
    # Get the paragraphs from the document with taskids
    old_pars = doc.get_paragraphs()
    for paragraph in old_pars:
        if not paragraph.get_attr('taskId'):
            old_pars.remove(paragraph)
    i = 1
    j = 0
    # For all blocks check if taskId is pluginnamehere, if it is find next available name.
    for p in blocks:
        if p.get_attr('taskId'):
            task_id = p.get_attr('taskId')
            if task_id == 'PLUGINNAMEHERE':
                task_id = 'Plugin' + str(i)
                while j < len(old_pars):
                    if task_id == old_pars[j].get_attr('taskId'):
                        i += 1
                        task_id = 'Plugin' + str(i)
                        j = 0
                    else:
                        j += 1
                p.set_attr('taskId', task_id)
                old_pars.append(p)
                j = 0
    return blocks


# Check new paragraphs with plugins for duplicate task ids
def check_duplicates(pars, doc, timdb):
    duplicates = []
    all_pars = doc.get_paragraphs()
    for paragraph in all_pars:
        if not paragraph.get_attr('taskId'):
            all_pars.remove(paragraph)
    for par in pars:
        if par.is_plugin():
            duplicate = []
            task_id = par.get_attr('taskId')
            count_of_same_task_ids = 0
            j = 0
            if len(all_pars) > 0:
                while j < len(all_pars):
                    if all_pars[j].get_attr('taskId') == task_id:
                        count_of_same_task_ids += 1
                        if count_of_same_task_ids > 1:
                            duplicate.append(task_id)
                            duplicate.append(par.get_id())
                            task_id_to_check = str(doc.doc_id) + "." + task_id
                            if timdb.answers.check_if_plugin_has_answers(task_id_to_check) == 1:
                                duplicate.append('hasAnswers')
                            duplicates.append(duplicate)
                            break
                    j += 1
    return duplicates


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
            timdb.readings.mark_read(getCurrentUserGroup(), doc, p)


@edit_page.route("/cancelChanges/", methods=["POST"])
def cancel_save_paragraphs():
    timdb = getTimDb()
    doc_id, original_par, new_pars, par_id = verify_json_params('docId', 'originalPar', 'newPars', 'parId')
    verify_edit_access(doc_id)
    doc = get_newest_document(doc_id)
    if len(new_pars) > 0:
        for new_par in new_pars:
            if not doc.has_paragraph(new_par):
                continue
            else:
                timdb.documents.delete_paragraph(doc, new_par)
    if original_par:
        timdb.documents.modify_paragraph(doc=doc,
                                         par_id=par_id,
                                         new_content=original_par.get('md'),
                                         new_attrs=original_par.get('attrs'),
                                         new_properties=original_par.get('props'))

    return jsonResponse({"status": "cancel"})


@edit_page.route("/newParagraphQ/", methods=["POST"])
def add_paragraphQ():
    md, doc_id, par_next_id = verify_json_params('text', 'docId', 'par_next')
    md = question_convert_js_to_yaml(md)
    return add_paragraph_common(md, doc_id, par_next_id)


@edit_page.route("/newParagraph/", methods=["POST"])
def add_paragraph():
    """Route for adding a new paragraph to a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.
    """
    md, doc_id, par_next_id = verify_json_params('text', 'docId', 'par_next')
    return add_paragraph_common(md, doc_id, par_next_id)


def add_paragraph_common(md, doc_id, par_next_id):
    timdb = getTimDb()
    verify_edit_access(doc_id)
    doc = get_newest_document(doc_id)
    try:
        editor_pars = get_pars_from_editor_text(doc, md)
    except ValidationException as e:
        return abort(400, str(e))

    editor_pars = check_and_rename_pluginnamehere(editor_pars, doc)

    pars = []
    separate_pars = DocumentParser(md).get_blocks()
    is_multi_block = len(separate_pars) > 1
    has_headers = None
    new_par_ids = []
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
        new_par_ids.append(par.get_id())
    mark_pars_as_read_if_chosen(pars, doc)

    notify_doc_owner(doc_id,
                     '[user_name] has edited your document [doc_name]',
                     '[user_name] has added a new paragraph on your document [doc_url]\n\n{}'.format(md),
                     setting="doc_modify", par_id=pars[0].get_id() if len(pars) > 0 else None,
                     group_id=get_group_id(), group_subject="Your document [doc_name] has been modified")

    return par_response(pars, doc, None, new_par_ids, update_cache=current_app.config['IMMEDIATE_PRELOAD'], edited=True)


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
                         text), setting="doc_modify",
                     group_id=get_group_id(), group_subject=get_group_subject())

    return par_response([], doc, update_cache=current_app.config['IMMEDIATE_PRELOAD'], edited=True)


@edit_page.route("/getUpdatedPars/<int:doc_id>")
def get_updated_pars(doc_id):
    """
    Gets updated paragraphs that were changed e.g. as the result of adding headings or modifying macros.
    :param doc_id: The document id.
    """
    verify_view_access(doc_id)
    return par_response([], Document(doc_id), update_cache=True)


@edit_page.route("/name_area/<int:doc_id>/<area_name>", methods=["POST"])
def name_area(doc_id, area_name):
    area_start, area_end = verify_json_params('area_start', 'area_end', require=True)
    (options,) = verify_json_params('options', require=True)

    verify_doc_exists(doc_id)
    verify_edit_access(doc_id)
    if not area_name or ' ' in area_name or '´' in area_name:
        abort(400, 'Invalid area name')

    doc = get_newest_document(doc_id)
    area_attrs = {'area': area_name}
    area_title = ''
    after_title = ''
    if options.get('collapsible'):
        area_attrs['collapse'] = 'true' if options.get('collapse') else 'false'
        if 'title' in options:
            hlevel = options.get('hlevel', 0)
            if ( hlevel ):
                area_title = ''.join(['#' for _ in range(0, hlevel)]) + ' ' + options['title']
            else:
                after_title = '\n' + options['title']

    if options.get('timed'):
        if options.get('starttime'):
            area_attrs['starttime'] = str(options.get('starttime'))
        if options.get('endtime'):
            area_attrs['endtime'] = str(options.get('endtime'))
        if options.get('alttext'):
            area_attrs['alttext'] = str(options.get('alttext'))

    doc.insert_paragraph(area_title + after_title, insert_before_id=area_start, attrs=area_attrs)
    doc.insert_paragraph('', insert_after_id=area_end, attrs={'area_end': area_name})

    return okJsonResponse()


@edit_page.route("/unwrap_area/<int:doc_id>/<area_name>", methods=["POST"])
def unwrap_area(doc_id, area_name):
    verify_doc_exists(doc_id)
    verify_edit_access(doc_id)
    if not area_name or ' ' in area_name or '´' in area_name:
        abort(400, 'Invalid area name')

    try:
        doc = get_newest_document(doc_id)
        area_pars = doc.get_named_section(area_name)

        # Remove the starting and ending paragraphs of the area
        doc.delete_paragraph(area_pars[0].get_id())
        doc.delete_paragraph(area_pars[len(area_pars) - 1].get_id())

    except TimDbException as e:
        abort(400, str(e))

    return okJsonResponse()
