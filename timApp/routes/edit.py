"""Routes for editing a document."""
from typing import List, Tuple, Optional

from flask import Blueprint, render_template
from flask import abort
from flask import current_app
from flask import request

from timApp.accesshelper import verify_edit_access, verify_view_access, get_rights, has_view_access
from timApp.common import post_process_pars
from timApp.dbaccess import get_timdb
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import Document
from timApp.documentmodel.documenteditresult import DocumentEditResult
from timApp.documentmodel.documentparser import ValidationException, ValidationWarning
from timApp.documentmodel.preloadoption import PreloadOption
from timApp.markdownconverter import md_to_html
from timApp.requesthelper import verify_json_params
from timApp.responsehelper import json_response, ok_response
from timApp.routes.editrequest import get_pars_from_editor_text, EditRequest
from timApp.routes.notify import notify_doc_watchers
from timApp.routes.qst import question_convert_js_to_yaml
from timApp.routes.view import get_module_ids
from timApp.sessioninfo import get_current_user_object, logged_in, get_current_user_group
from timApp.synchronize_translations import synchronize_translations
from timApp.timdb.bookmarks import Bookmarks
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.notification import NotificationType
from timApp.timdb.timdbexception import TimDbException
from timApp.utils import get_error_html
from timApp.validation import validate_uploaded_document_content

edit_page = Blueprint('edit_page',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@edit_page.route('/update/<int:doc_id>', methods=['POST'])
def update_document(doc_id):
    """Route for updating a document as a whole.

    :param doc_id: The id of the document to be modified.
    :return: A JSON object containing the versions of the document.

    """
    timdb = get_timdb()
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    if not docentry:
        abort(404)
    verify_edit_access(doc_id)
    if 'file' in request.files:
        file = request.files['file']
        content = validate_uploaded_document_content(file)
        original = request.form['original']
        strict_validation = not request.form.get('ignore_warnings', False)
    elif 'template_name' in request.get_json():
        template = DocEntry.find_by_path(request.get_json()['template_name'], try_translation=True)
        if not has_view_access(template.id):
            abort(403, 'Permission denied')
        doc = template.document
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
        return json_response({'message': 'Failed to convert the file to UTF-8.'}, 400)
    doc = docentry.document_as_current_user
    doc.preload_option = PreloadOption.all
    ver_before = doc.get_version()
    try:
        # To verify view rights for possible referenced paragraphs, we call this first:
        editor_pars = get_pars_from_editor_text(doc, content, break_on_elements=True, skip_access_check=True)
        edit_result = timdb.documents.update_document(doc, content, original, strict_validation)
        check_and_rename_pluginnamehere(editor_pars, doc)
        old_pars = doc.get_paragraphs()
        i = 0
        while i < len(editor_pars):
            if editor_pars[i].get_attr('taskId') and old_pars[i].get_attr('taskId'):
                if editor_pars[i].get_attr('taskId') != old_pars[i].get_attr('taskId'):
                    timdb.documents.modify_paragraph(doc=doc,
                                                     par_id=old_pars[i].get_id(),
                                                     new_content=old_pars[i].get_markdown(),
                                                     new_attrs=editor_pars[i].get_attrs())
            i += 1
        synchronize_translations(docentry, edit_result)
    except ValidationWarning as e:
        return json_response({'error': str(e), 'is_warning': True}, status_code=400)
    except (TimDbException, ValidationException) as e:
        return abort(400, str(e))
    pars = doc.get_paragraphs()
    return manage_response(docentry, pars, timdb, ver_before)


def manage_response(docentry: DocInfo, pars: List[DocParagraph], timdb, ver_before: Tuple[int, int]):
    doc = docentry.document_as_current_user
    duplicates = check_duplicates(pars, doc, timdb)
    chg = doc.get_changelog()
    for ver in chg:
        ver['group'] = timdb.users.get_user_group_name(ver.pop('group_id'))
    notify_doc_watchers(docentry,
                        get_diff_link(docentry, ver_before),
                        NotificationType.DocModified)
    return json_response({'versions': chg, 'fulltext': doc.export_markdown(), 'duplicates': duplicates})


def get_diff_link(docentry: DocInfo, ver_before):
    ver_after = docentry.document.get_version()
    return """Link to changes: {}/diff/{}/{}/{}/{}/{}\n\n""".format(current_app.config['TIM_HOST'], docentry.id,
                                                                ver_before[0], ver_before[1],
                                                                ver_after[0], ver_after[1])


@edit_page.route("/postNewTaskNames/", methods=['POST'])
def rename_task_ids():
    timdb = get_timdb()
    doc_id, duplicates = verify_json_params('docId', 'duplicates')
    manage_view = verify_json_params('manageView', require=False, default=False)
    verify_edit_access(doc_id)
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    if not docentry:
        abort(404)
    doc = docentry.document_as_current_user
    ver_before = doc.get_version()
    pars = []
    old_pars = doc.get_paragraphs()

    # Get paragraphs with taskIds
    for paragraph in old_pars:
        if not paragraph.is_task():
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
                                                        new_attrs=attrs)
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
        return manage_response(docentry, pars, timdb, ver_before)


@edit_page.route("/postParagraphQ/", methods=['POST'])
def modify_paragraph_q():
    """Route for modifying a question editor paragraph in a document.

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
    """Route for modifying a paragraph in a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.

    """
    doc_id, md, par_id, par_next_id = verify_json_params('docId', 'text', 'par', 'par_next')
    return modify_paragraph_common(doc_id, md, par_id, par_next_id)


def modify_paragraph_common(doc_id, md, par_id, par_next_id):
    timdb = get_timdb()
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    if not docentry:
        abort(404)
    verify_edit_access(doc_id)

    doc = docentry.document_as_current_user
    if not doc.has_paragraph(par_id):
        abort(400, 'Paragraph not found: ' + par_id)

    edit_request = EditRequest.from_request(doc, text=md)
    area_start = edit_request.area_start
    area_end = edit_request.area_end
    editing_area = edit_request.editing_area
    try:
        editor_pars = edit_request.get_pars(skip_access_check=True)
    except ValidationException as e:
        return abort(400, str(e))

    editor_pars = check_and_rename_pluginnamehere(editor_pars, doc)

    if editing_area:
        try:
            new_start, new_end, edit_result = doc.update_section(md, area_start, area_end)
            pars = doc.get_section(new_start, new_end)
        except (ValidationException, TimDbException) as e:
            return abort(400, str(e))
    else:
        edit_result = DocumentEditResult()
        original_par = doc.get_paragraph(par_id)
        pars = []

        if editor_pars[0].is_different_from(original_par):
            [par], _ = timdb.documents.modify_paragraph(doc=doc,
                                                        par_id=par_id,
                                                        new_content=editor_pars[0].get_markdown(),
                                                        new_attrs=editor_pars[0].get_attrs())
            pars.append(par)
            edit_result.changed.append(par)
        else:
            # If the first paragraph was not modified at all, append the original one
            pars.append(original_par)
        for p in editor_pars[1:]:
            [par], _ = timdb.documents.add_paragraph(doc, p.get_markdown(), par_next_id, attrs=p.get_attrs())
            pars.append(par)
            edit_result.added.append(par)

    mark_pars_as_read_if_chosen(pars, doc)

    synchronize_translations(docentry, edit_result)
    notify_doc_watchers(docentry,
                        get_diff_link(docentry, edit_request.old_doc_version) + 'Paragraph was edited:\n\n' + md,
                        NotificationType.DocModified,
                        par=pars[0] if pars else None)
    return par_response(pars,
                        doc,
                        update_cache=current_app.config['IMMEDIATE_PRELOAD'],
                        edit_request=edit_request,
                        edit_result=edit_result)


@edit_page.route("/preview/<int:doc_id>", methods=['POST'])
def preview_paragraphs(doc_id):
    """Route for previewing paragraphs.

    :param doc_id: The id of the document in which the preview will be rendered.
    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.

    """
    text, = verify_json_params('text')
    rjson = request.get_json()
    if not rjson.get('isComment'):
        doc = Document(doc_id)
        edit_request = EditRequest.from_request(doc, preview=True)
        try:
            blocks = edit_request.get_pars()
            for par in blocks:
                if par.is_question():
                    par.set_attr('isQuestion', par.is_question())
                    par.set_attr('question', False)
                    par.set_attr('plugin', 'qst')
            return par_response(blocks, doc, edit_request=edit_request)
        except Exception as e:
            err_html = get_error_html(e)
            blocks = [DocParagraph.create(doc=doc, md='', html=err_html)]
            return par_response(blocks, doc)
    else:
        return json_response({'texts': md_to_html(text), 'js': [], 'css': []})


def par_response(pars,
                 doc,
                 update_cache=False,
                 edit_request: Optional[EditRequest]=None,
                 edit_result: Optional[DocumentEditResult]=None):
    current_user = get_current_user_object()
    if update_cache:
        changed_pars = DocParagraph.preload_htmls(doc.get_paragraphs(),
                                                  doc.get_settings(),
                                                  persist=update_cache)
    else:
        changed_pars = []
        ctx = None
        if edit_request:
            ctx = edit_request.get_context_par()

            # If the document was changed, there is no HTML cache for the new version, so we "cheat" by lying the document
            # version so that the preload_htmls call is still fast.
            if edit_result:
                for p in pars:
                    assert p.doc is doc
                doc.version = edit_request.old_doc_version
            doc.insert_temporary_pars(edit_request.get_pars(), ctx)

        DocParagraph.preload_htmls(pars, doc.get_settings(current_user), context_par=ctx,
                                   persist=update_cache)

    if edit_result:
        preview = False
    else:
        preview = edit_request and edit_request.preview
    # Do not check for duplicates for preview because the operation is heavy
    if not preview:
        duplicates = check_duplicates(pars, doc, get_timdb())
        if edit_request and logged_in():
            bms = Bookmarks(get_current_user_object())
            d = DocEntry.find_by_id(doc.doc_id, try_translation=True)
            if d is not None:
                bms.add_bookmark('Last edited',
                                 d.title,
                                 '/view/' + d.path,
                                 move_to_top=True,
                                 limit=current_app.config['LAST_EDITED_BOOKMARK_LIMIT']).save_bookmarks()
    else:
        duplicates = None

    pars, js_paths, css_paths, modules = post_process_pars(doc, pars, current_user, edit_window=preview)

    changed_pars, _, _, _ = post_process_pars(doc, changed_pars, current_user, edit_window=preview)
    original_par = edit_request.original_par if edit_request else None

    return json_response({'texts': render_template('partials/paragraphs.html',
                                                   text=pars,
                                                   item={'rights': get_rights(doc.doc_id)},
                                                   preview=preview),
                          'js': js_paths,
                          'jsModuleIds': list(get_module_ids(js_paths)),
                          'css': css_paths,
                          'angularModule': modules,  # not used in JS at all, maybe not needed at all
                          'changed_pars': {p['id']: render_template('partials/paragraphs.html',
                                                                    text=[p],
                                                                    item={'rights': get_rights(doc.doc_id)}) for p in
                                           changed_pars},
                          'version': doc.get_version(),
                          'duplicates': duplicates,
                          'original_par': {'md': original_par.get_markdown(),
                                           'attrs': original_par.get_attrs()} if original_par else None,
                          'new_par_ids': edit_result.new_par_ids if edit_result else None
                          })


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
                text_part = task_id[:i + 1]
                task_id_body = text_part
                if i < len(task_id) - 1:
                    task_id_number = int(task_id[(i + 1):])
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
def check_and_rename_pluginnamehere(blocks: List[DocParagraph], doc: Document):
    # Get the paragraphs from the document with taskids
    old_pars = None # lazy load for old_pars
    i = 1
    j = 0
    # For all blocks check if taskId is pluginnamehere, if it is find next available name.
    for p in blocks: # go thru all new pars if they need to be renamed
        if p.is_task():
            task_id = p.get_attr('taskId')
            if task_id == 'PLUGINNAMEHERE':
                if old_pars is None:  # now old_pars is needed, load them once
                    pars = doc.get_paragraphs()
                    old_pars = []
                    for paragraph in pars:
                        if not paragraph.is_task():
                            old_pars.append(paragraph)

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
    all_pars = None # cache all_pars
    for par in pars:
        if par.is_task():
            if all_pars is None: # now we need the pars
                docpars = doc.get_paragraphs()
                all_pars = []
                for paragraph in docpars:
                    if paragraph.is_task():
                        all_pars.append(paragraph)

            duplicate = []
            task_id = par.get_attr('taskId')
            par_id = par.get_id()
            count_of_same_task_ids = 0
            j = 0
            while j < len(all_pars):
                if all_pars[j].get_id() != par_id and all_pars[j].get_attr('taskId') == task_id:  # count not self
                    count_of_same_task_ids += 1
                    if count_of_same_task_ids > 0:
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
    timdb = get_timdb()
    if mark_read:
        for p in pars:
            timdb.readings.mark_read(get_current_user_group(), doc, p)


@edit_page.route("/cancelChanges/", methods=["POST"])
def cancel_save_paragraphs():
    timdb = get_timdb()
    doc_id, original_par, new_pars, par_id = verify_json_params('docId', 'originalPar', 'newPars', 'parId')
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    if not docentry:
        abort(404)
    verify_edit_access(doc_id)
    doc = docentry.document_as_current_user
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
                                         new_attrs=original_par.get('attrs'))

    return json_response({"status": "cancel"})


@edit_page.route("/newParagraphQ/", methods=["POST"])
def add_paragraph_q():
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
    timdb = get_timdb()
    verify_edit_access(doc_id)
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    if not docentry:
        abort(404)
    doc = docentry.document_as_current_user
    edit_result = DocumentEditResult()
    edit_request = EditRequest.from_request(doc, md)
    try:
        editor_pars = edit_request.get_pars()
    except ValidationException as e:
        return abort(400, str(e))

    editor_pars = check_and_rename_pluginnamehere(editor_pars, doc)

    pars = []
    for p in editor_pars:
        [par], _ = timdb.documents.add_paragraph(doc, p.get_markdown(), par_next_id, attrs=p.get_attrs())
        pars.append(par)
        edit_result.added.append(par)
    mark_pars_as_read_if_chosen(pars, doc)

    synchronize_translations(docentry, edit_result)
    if pars:
        notify_doc_watchers(docentry,
                            get_diff_link(docentry, edit_request.old_doc_version) + 'Paragraph was added:\n\n' + md,
                            NotificationType.DocModified,
                            par=pars[0])
    return par_response(pars,
                        doc,
                        update_cache=current_app.config['IMMEDIATE_PRELOAD'],
                        edit_result=edit_result,
                        edit_request=edit_request)


@edit_page.route("/deleteParagraph/<int:doc_id>", methods=["POST"])
def delete_paragraph(doc_id):
    """Route for deleting a paragraph from a document.

    :param doc_id: The id of the document.
    :return: A JSON object containing the version of the new document.

    """
    timdb = get_timdb()
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    if not docentry:
        abort(404)
    verify_edit_access(doc_id)
    area_start, area_end = verify_json_params('area_start', 'area_end', require=False)
    doc = docentry.document_as_current_user
    version_before = doc.get_version()
    if area_end and area_start:
        for p in (area_start, area_end):
            if not doc.has_paragraph(p):
                abort(400, 'Paragraph {} does not exist'.format(p))
        md = doc.export_section(area_start, area_end)
        edit_result = doc.delete_section(area_start, area_end)
    else:
        par_id, = verify_json_params('par')
        if not doc.has_paragraph(par_id):
            abort(400, 'Paragraph {} does not exist'.format(par_id))
        par = doc.get_paragraph(par_id)
        md = par.get_markdown()
        timdb.documents.delete_paragraph(doc, par_id)
        edit_result = DocumentEditResult(deleted=[par])

    synchronize_translations(docentry, edit_result)
    notify_doc_watchers(docentry,
                        get_diff_link(docentry, version_before) + 'Paragraph was deleted:\n\n' + md,
                        NotificationType.DocModified)
    return par_response([],
                        doc,
                        update_cache=current_app.config['IMMEDIATE_PRELOAD'],
                        edit_result=edit_result)


@edit_page.route("/getUpdatedPars/<int:doc_id>")
def get_updated_pars(doc_id):
    """Gets updated paragraphs that were changed e.g. as the result of adding headings or modifying macros.

    :param doc_id: The document id.

    """
    verify_view_access(doc_id)
    return par_response([], Document(doc_id, preload_option=PreloadOption.all), update_cache=True)


@edit_page.route("/name_area/<int:doc_id>/<area_name>", methods=["POST"])
def name_area(doc_id, area_name):
    area_start, area_end = verify_json_params('area_start', 'area_end', require=True)
    (options,) = verify_json_params('options', require=True)

    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    if not docentry:
        abort(404)
    verify_edit_access(doc_id)
    if not area_name or ' ' in area_name or '´' in area_name:
        abort(400, 'Invalid area name')

    doc = docentry.document_as_current_user
    area_attrs = {'area': area_name}
    area_title = ''
    after_title = ''
    if options.get('collapsible'):
        area_attrs['collapse'] = 'true' if options.get('collapse') else 'false'
        if 'title' in options:
            hlevel = options.get('hlevel', 0)
            if hlevel:
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

    return ok_response()


@edit_page.route("/unwrap_area/<int:doc_id>/<area_name>", methods=["POST"])
def unwrap_area(doc_id, area_name):
    docentry = DocEntry.find_by_id(doc_id, try_translation=True)
    if not docentry:
        abort(404)
    verify_edit_access(doc_id)
    if not area_name or ' ' in area_name or '´' in area_name:
        abort(400, 'Invalid area name')

    try:
        doc = docentry.document_as_current_user
        area_pars = doc.get_named_section(area_name)

        # Remove the starting and ending paragraphs of the area
        doc.delete_paragraph(area_pars[0].get_id())
        doc.delete_paragraph(area_pars[len(area_pars) - 1].get_id())

    except TimDbException as e:
        abort(400, str(e))

    return ok_response()
