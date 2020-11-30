"""Routes for editing a document."""
import re
from dataclasses import dataclass
from typing import List, Optional

from flask import Blueprint, render_template
from flask import abort
from flask import current_app
from flask import request

from timApp.admin.associate_old_uploads import upload_regexes
from timApp.answer.answer import Answer
from timApp.auth.accesshelper import verify_edit_access, verify_view_access, get_rights, get_doc_or_abort, \
    verify_teacher_access, verify_manage_access, verify_ownership, verify_seeanswers_access
from timApp.auth.sessioninfo import get_current_user_object, logged_in, get_current_user_group, \
    user_context_with_logged_in
from timApp.bookmark.bookmarks import Bookmarks
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document, get_duplicate_id_msg
from timApp.document.editing.documenteditresult import DocumentEditResult
from timApp.document.editing.editrequest import get_pars_from_editor_text, EditRequest
from timApp.document.editing.proofread import proofread_pars, process_spelling_errors
from timApp.document.exceptions import ValidationException, ValidationWarning
from timApp.document.hide_names import is_hide_names
from timApp.document.post_process import post_process_pars
from timApp.document.preloadoption import PreloadOption
from timApp.document.translation.synchronize_translations import synchronize_translations
from timApp.document.version import Version
from timApp.document.viewcontext import ViewRoute, ViewContext, default_view_ctx
from timApp.item.validation import validate_uploaded_document_content
from timApp.markdown.markdownconverter import md_to_html
from timApp.notification.notification import NotificationType
from timApp.notification.notify import notify_doc_watchers
from timApp.plugin.plugin import Plugin
from timApp.plugin.qst.qst import question_convert_js_to_yaml
from timApp.plugin.save_plugin import save_plugin
from timApp.readmark.readings import mark_read
from timApp.timdb.dbaccess import get_timdb
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.upload.uploadedfile import UploadedFile
from timApp.util.flask.requesthelper import verify_json_params, use_model
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.utils import get_error_html

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
    docentry = get_doc_or_abort(doc_id)
    verify_edit_access(docentry)
    if 'file' in request.files:
        file = request.files['file']
        content = validate_uploaded_document_content(file)
        original = request.form['original']
        strict_validation = not request.form.get('ignore_warnings', False)
    elif 'template_name' in request.get_json():
        template = DocEntry.find_by_path(request.get_json()['template_name'])
        if not template:
            abort(404, 'Template not found')
        verify_view_access(template)
        content = template.document.export_markdown()
        if content == '':
            return abort(400, 'The selected template is empty.')
        existing_pars = docentry.document_as_current_user.get_paragraphs()
        if existing_pars:
            abort(400, 'Cannot load a template because the document is not empty.')
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

        # TODO: Access check should be more fine-grained. Should only check pars that were actually edited.
        for p in doc.get_paragraphs():
            verify_par_edit_access(p)

        _, _, edit_result = doc.update(content, original, strict_validation)
        check_and_rename_pluginnamehere(editor_pars, doc)
        old_pars = doc.get_paragraphs()
        for op, ep in zip(old_pars, editor_pars):
            if ep.get_attr('taskId') and op.get_attr('taskId'):
                if ep.get_attr('taskId') != op.get_attr('taskId'):
                    p = doc.modify_paragraph(par_id=op.get_id(),
                                             new_text=op.get_markdown(),
                                             new_attrs=ep.get_attrs())
                    edit_result.changed.append(p)
        if not edit_result.empty:
            docentry.update_last_modified()
            db.session.commit()
        synchronize_translations(docentry, edit_result)
    except ValidationWarning as e:
        return json_response({'error': str(e), 'is_warning': True}, status_code=400)
    except (TimDbException, ValidationException) as e:
        return abort(400, str(e))
    pars = doc.get_paragraphs()
    return manage_response(docentry, pars, timdb, ver_before)


def manage_response(docentry: DocInfo, pars: List[DocParagraph], timdb, ver_before: Version):
    doc = docentry.document_as_current_user
    chg = doc.get_changelog()
    notify_doc_watchers(docentry, '', NotificationType.DocModified, old_version=ver_before)
    duplicates = check_duplicates(pars, doc)
    db.session.commit()
    return json_response({'versions': chg, 'fulltext': doc.export_markdown(), 'duplicates': duplicates})


@edit_page.route("/postNewTaskNames/", methods=['POST'])
def rename_task_ids():
    timdb = get_timdb()
    doc_id, duplicates = verify_json_params('docId', 'duplicates')
    manage_view = verify_json_params('manageView', require=False, default=False)
    docinfo = get_doc_or_abort(doc_id)
    verify_edit_access(docinfo)
    doc = docinfo.document_as_current_user
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
            verify_par_edit_access(original_par)
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
            par = doc.modify_paragraph(par_id=duplicate[2],
                                       new_text=original_par.get_markdown(),
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
                            docinfo,
                            spellcheck=False,
                            update_cache=current_app.config['IMMEDIATE_PRELOAD'])
    else:
        return manage_response(docinfo, pars, timdb, ver_before)


@edit_page.route("/postParagraphQ/", methods=['POST'])
def modify_paragraph_q():
    """Route for modifying a question editor paragraph in a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.

    """
    question_data, doc_id, par_id, is_task = verify_json_params('question', 'docId', 'par', 'isTask')
    task_id, = verify_json_params('taskId', require=False)
    md = question_convert_js_to_yaml(question_data, is_task, task_id)
    ret = modify_paragraph_common(doc_id, md, par_id, par_next_id=None)
    return ret


@edit_page.route("/postParagraph/", methods=['POST'])
def modify_paragraph():
    """Route for modifying a paragraph in a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.

    """
    doc_id, md, par_id = verify_json_params('docId', 'text', 'par')
    par_next_id, = verify_json_params('par_next', require=False)
    return modify_paragraph_common(doc_id, md, par_id, par_next_id)


def verify_par_edit_access(par: DocParagraph):
    """Verifies that the current user has edit access to the specified DocParagraph."""
    edit_attr = par.get_attr('edit', 'edit')
    message = f'Only users with {edit_attr} access can edit this paragraph ({par.get_id()}).'
    d = par.doc.get_docinfo()
    if edit_attr == 'edit':
        verify_edit_access(d, message=message)
    elif edit_attr == 'teacher':
        verify_teacher_access(d, message=message)
    elif edit_attr == 'see_answers':
        verify_seeanswers_access(d, message=message)
    elif edit_attr == 'manage':
        verify_manage_access(d, message=message)
    elif edit_attr == 'owner':
        verify_ownership(d, message=message)


def modify_paragraph_common(doc_id: int, md: str, par_id: str, par_next_id: Optional[str]):
    docinfo = get_doc_or_abort(doc_id)
    verify_edit_access(docinfo)

    doc = docinfo.document_as_current_user

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
            curr_section = doc.get_section(area_start, area_end)
            for p in curr_section:
                verify_par_edit_access(p)
            new_start, new_end, edit_result = doc.update_section(md, area_start, area_end)
            pars = doc.get_section(new_start, new_end)
        except (ValidationException, TimDbException) as e:
            return abort(400, str(e))
    else:
        try:
            original_par = doc.get_paragraph(par_id)
        except TimDbException as e:
            return abort(404, str(e))
        edit_result = DocumentEditResult()
        pars = []
        pars_to_add = editor_pars[1:]
        abort_if_duplicate_ids(doc, pars_to_add)

        p = editor_pars[0]
        tr_opt = edit_request.mark_translated
        if tr_opt is None:
            pass
        elif tr_opt:
            if p.is_translation():
                deref = mark_as_translated(p)
                if not deref:
                    return abort(400, 'Paragraph is not a translation.')
        else:
            p.set_attr('rt', None)

        if p.is_different_from(original_par):
            verify_par_edit_access(original_par)
            par = doc.modify_paragraph_obj(par_id=par_id,
                                           p=p)
            pars.append(par)
            edit_result.changed.append(par)
        else:
            # If the first paragraph was not modified at all, append the original one
            pars.append(original_par)
        for p in pars_to_add:
            par = doc.insert_paragraph_obj(p, insert_before_id=par_next_id)
            pars.append(par)
            edit_result.added.append(par)
        if not edit_result.empty:
            docinfo.update_last_modified()

    mark_pars_as_read_if_chosen(pars, doc)

    synchronize_translations(docinfo, edit_result)
    notify_doc_watchers(docinfo, md, NotificationType.ParModified, par=pars[0] if pars else None,
                        old_version=edit_request.old_doc_version)
    return par_response(pars,
                        docinfo,
                        spellcheck=False,
                        update_cache=current_app.config['IMMEDIATE_PRELOAD'],
                        edit_request=edit_request,
                        edit_result=edit_result)


def mark_as_translated(p: DocParagraph):
    try:
        deref = p.get_referenced_pars()
    except TimDbException:
        deref = None
    if deref:
        p.set_attr('rt', deref[0].get_hash())
    return deref


def abort_if_duplicate_ids(doc: Document, pars_to_add: List[DocParagraph]):
    conflicting_ids = set(p.get_id() for p in pars_to_add) & set(doc.get_par_ids())
    if conflicting_ids:
        abort(400, get_duplicate_id_msg(conflicting_ids))


@edit_page.route("/preview/<int:doc_id>", methods=['POST'])
def preview_paragraphs(doc_id):
    """Route for previewing paragraphs.

    :param doc_id: The id of the document in which the preview will be rendered.
    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.

    """
    text, = verify_json_params('text')
    proofread, = verify_json_params('proofread', require=False, default=False)
    docinfo = get_doc_or_abort(doc_id)
    rjson = request.get_json()
    if not rjson.get('isComment'):
        doc = docinfo.document
        edit_request = EditRequest.from_request(doc, preview=True)
        try:
            blocks = edit_request.get_pars()
        except ValidationException as e:
            blocks = [DocParagraph.create(doc=doc, md='', html=get_error_html(e))]
            proofread = False
            edit_request = None
        return par_response(blocks, docinfo, proofread, edit_request=edit_request)
    else:
        comment_html = md_to_html(text)
        if proofread:
            comment_html = process_spelling_errors(comment_html).new_html
        return json_response({'texts': comment_html, 'js': [], 'css': []})


def update_associated_uploads(pars: List[DocParagraph], doc: DocInfo):
    for p in pars:
        md = p.get_markdown()
        for r in upload_regexes:
            c = re.compile(r, re.DOTALL)
            for m in c.finditer(md):
                u_id = m.group('id')
                name = m.group('name')
                up = UploadedFile.get_by_id_and_filename(int(u_id), name)
                if not up:
                    continue
                if not verify_manage_access(up, check_parents=True, require=False):
                    continue
                if up.block in doc.children:
                    continue
                doc.children.append(up.block)


def par_response(pars: List[DocParagraph],
                 docu: DocInfo,
                 spellcheck=False,
                 update_cache=False,
                 edit_request: Optional[EditRequest] = None,
                 edit_result: Optional[DocumentEditResult] = None):
    user_ctx = user_context_with_logged_in(None)
    doc = docu.document
    new_doc_version = doc.get_version()

    if edit_result:
        preview = False
    else:
        preview = bool(edit_request and edit_request.preview)
    if edit_request:
        view_ctx = ViewContext(edit_request.viewname or ViewRoute.View, preview, hide_names_requested=is_hide_names())
    else:
        view_ctx = ViewContext(ViewRoute.View, preview, hide_names_requested=is_hide_names())
    if update_cache:
        changed_pars = DocParagraph.preload_htmls(
            doc.get_paragraphs(include_preamble=True),
            doc.get_settings(),
            view_ctx,
            persist=update_cache,
        )
    else:
        changed_pars = []
        ctx = None
        settings = doc.get_settings()
        if edit_request:
            ctx = edit_request.context_par

            # If the document was changed, there is no HTML cache for the new version, so we "cheat" by lying the document
            # version so that the preload_htmls call is still fast.
            if edit_result:
                for p in pars:
                    assert p.doc is doc
                doc.version = edit_request.old_doc_version
            doc.insert_temporary_pars(edit_request.get_pars(), ctx)

        DocParagraph.preload_htmls(pars, settings, view_ctx, context_par=ctx, persist=update_cache)
    trdiff = None
    # Do not check for duplicates for preview because the operation is heavy
    if not preview:
        duplicates = check_duplicates(pars, doc)
        if edit_request and logged_in():
            update_associated_uploads(pars, docu)
            if current_app.config['BOOKMARKS_ENABLED']:
                bms = Bookmarks(get_current_user_object())
                bms.add_bookmark(
                    'Last edited',
                    docu.title,
                    docu.get_relative_url_for_view((edit_request.viewname or ViewRoute.View).value),
                    move_to_top=True,
                    limit=current_app.config['LAST_EDITED_BOOKMARK_LIMIT'],
                ).save_bookmarks()
    else:
        duplicates = None
        if len(pars) == 1:
            p = pars[0]
            if p.is_translation():
                try:
                    deref = p.get_referenced_pars()[0].ref_chain
                except TimDbException:
                    pass
                else:
                    newest_exported = deref.get_exported_markdown()
                    old_exported = ''
                    rt = p.get_attr('rt')
                    if rt:
                        try:
                            old_par = DocParagraph.get(deref.doc, deref.get_id(), rt)
                        except TimDbException:
                            pass
                        else:
                            old_exported = old_par.get_exported_markdown()
                    trdiff = {'old': old_exported, 'new': newest_exported}
    post_process_result = post_process_pars(doc, pars, user_ctx, view_ctx)

    changed_post_process_result = post_process_pars(doc, changed_pars, user_ctx, view_ctx)
    original_par = edit_request.original_par if edit_request else None

    if spellcheck:
        proofed_text = proofread_pars(post_process_result.texts)
        for p, r in zip(post_process_result.texts, proofed_text):
            p['html'] = r.new_html

    r = json_response({'texts': render_template('partials/paragraphs.html',
                                                text=post_process_result.texts,
                                                item={'rights': get_rights(doc.get_docinfo())},
                                                preview=preview),
                       'js': post_process_result.js_paths,
                       'css': post_process_result.css_paths,
                       'trdiff': trdiff,
                       'changed_pars': {p['id']: render_template('partials/paragraphs.html',
                                                                 text=[p],
                                                                 item={'rights': get_rights(doc.get_docinfo())}) for p
                                        in
                                        changed_post_process_result.texts},
                       'version': new_doc_version,
                       'duplicates': duplicates,
                       'original_par': {'md': original_par.get_markdown(),
                                        'attrs': original_par.get_attrs()} if original_par else None,
                       'new_par_ids': edit_result.new_par_ids if edit_result else None,
                       })
    db.session.commit()
    return r


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
    old_pars = None  # lazy load for old_pars
    i = 1
    j = 0
    # For all blocks check if taskId is pluginnamehere, if it is find next available name.
    for p in blocks:  # go thru all new pars if they need to be renamed
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
def check_duplicates(pars, doc):
    duplicates = []
    all_pars = None  # cache all_pars
    for par in pars:
        if par.is_task():
            if all_pars is None:  # now we need the pars
                doc.clear_mem_cache()
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
                        if Answer.query.filter_by(task_id=task_id_to_check).first():
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
    mr = request.get_json().get('tags', {}).get('markread')
    if mr:
        for p in pars:
            mark_read(get_current_user_group(), doc, p)


@edit_page.route("/cancelChanges/", methods=["POST"])
def cancel_save_paragraphs():
    doc_id, original_par, new_pars, par_id = verify_json_params('docId', 'originalPar', 'newPars', 'parId')
    docentry = get_doc_or_abort(doc_id)
    verify_edit_access(docentry)
    doc = docentry.document_as_current_user
    for new_par in new_pars:
        try:
            par = doc.get_paragraph(new_par)
        except TimDbException:
            continue
        else:
            verify_par_edit_access(par)
            doc.delete_paragraph(new_par)
    if original_par:
        orig = doc.get_paragraph(par_id)
        verify_par_edit_access(orig)
        doc.modify_paragraph(par_id=par_id,
                             new_text=original_par.get('md'),
                             new_attrs=original_par.get('attrs'))

    return json_response({"status": "cancel"})


@edit_page.route("/newParagraphQ/", methods=["POST"])
def add_paragraph_q():
    question_data, doc_id, par_next_id, is_task = verify_json_params('question', 'docId', 'par_next', 'isTask')
    task_id, = verify_json_params('taskId', require=False)
    md = question_convert_js_to_yaml(question_data, is_task, task_id)
    return add_paragraph_common(md, doc_id, par_next_id)


@edit_page.route("/newParagraph/", methods=["POST"])
def add_paragraph():
    """Route for adding a new paragraph to a document.

    :return: A JSON object containing the paragraphs in HTML form along with JS, CSS and Angular module dependencies.

    """
    md, doc_id = verify_json_params('text', 'docId')
    par_next_id, = verify_json_params('par_next', require=False)
    return add_paragraph_common(md, doc_id, par_next_id)


def add_paragraph_common(md: str, doc_id: int, par_next_id: Optional[str]):
    docinfo = get_doc_or_abort(doc_id)
    verify_edit_access(docinfo)
    doc = docinfo.document_as_current_user
    if par_next_id and not doc.has_paragraph(par_next_id):
        abort(400, doc.get_par_not_found_msg(par_next_id))
    edit_result = DocumentEditResult()
    edit_request = EditRequest.from_request(doc, md)
    try:
        editor_pars = edit_request.get_pars()
    except ValidationException as e:
        return abort(400, str(e))

    abort_if_duplicate_ids(doc, editor_pars)

    editor_pars = check_and_rename_pluginnamehere(editor_pars, doc)

    pars = []
    for p in editor_pars:
        par = doc.insert_paragraph_obj(p, insert_before_id=par_next_id)
        pars.append(par)
        edit_result.added.append(par)
    if not edit_result.empty:
        docinfo.update_last_modified()
    mark_pars_as_read_if_chosen(pars, doc)

    synchronize_translations(docinfo, edit_result)
    if pars:
        notify_doc_watchers(docinfo, md, NotificationType.ParAdded, par=pars[0],
                            old_version=edit_request.old_doc_version)
    return par_response(pars,
                        docinfo,
                        spellcheck=False,
                        update_cache=current_app.config['IMMEDIATE_PRELOAD'],
                        edit_result=edit_result,
                        edit_request=edit_request)


@edit_page.route("/deleteParagraph/<int:doc_id>", methods=["POST"])
def delete_paragraph(doc_id):
    """Route for deleting a paragraph from a document.

    :param doc_id: The id of the document.
    :return: A JSON object containing the version of the new document.

    """
    docinfo = get_doc_or_abort(doc_id)
    verify_edit_access(docinfo)
    area_start, area_end = verify_json_params('area_start', 'area_end', require=False)
    doc = docinfo.document_as_current_user
    version_before = doc.get_version()
    if area_end and area_start:
        for p in (area_start, area_end):
            if not doc.has_paragraph(p):
                abort(400, f'Paragraph {p} does not exist')
        md = doc.export_section(area_start, area_end)
        curr_section = doc.get_section(area_start, area_end)
        for p in curr_section:
            verify_par_edit_access(p)
        edit_result = doc.delete_section(area_start, area_end)
    else:
        par_id, = verify_json_params('par')
        if not doc.has_paragraph(par_id):
            abort(400, f'Paragraph {par_id} does not exist')
        par = doc.get_paragraph(par_id)
        verify_par_edit_access(par)
        md = par.get_markdown()
        doc.delete_paragraph(par_id)
        edit_result = DocumentEditResult(deleted=[par])

    if not edit_result.empty:
        docinfo.update_last_modified()
    synchronize_translations(docinfo, edit_result)
    notify_doc_watchers(docinfo, md, NotificationType.ParDeleted, old_version=version_before)
    return par_response([],
                        docinfo,
                        spellcheck=False,
                        update_cache=current_app.config['IMMEDIATE_PRELOAD'],
                        edit_result=edit_result)


@edit_page.route("/getUpdatedPars/<int:doc_id>")
def get_updated_pars(doc_id):
    """Gets updated paragraphs that were changed e.g. as the result of adding headings or modifying macros.

    :param doc_id: The document id.

    """
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    d.document.preload_option = PreloadOption.all
    return par_response([], d, spellcheck=False, update_cache=True)


@edit_page.route("/name_area/<int:doc_id>/<area_name>", methods=["POST"])
def name_area(doc_id, area_name):
    area_start, area_end = verify_json_params('area_start', 'area_end', require=True)
    (options,) = verify_json_params('options', require=True)

    docentry = get_doc_or_abort(doc_id)
    verify_edit_access(docentry)
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
    docentry = get_doc_or_abort(doc_id)
    verify_edit_access(docentry)
    if not area_name or ' ' in area_name or '´' in area_name:
        abort(400, 'Invalid area name')

    try:
        doc = docentry.document_as_current_user
        area_pars = doc.get_named_section(area_name)

        # Remove the starting and ending paragraphs of the area
        area_start = area_pars[0]
        area_end = area_pars[-1]
        verify_par_edit_access(area_start)
        verify_par_edit_access(area_end)
        doc.delete_paragraph(area_start.get_id())
        doc.delete_paragraph(area_end.get_id())

    except TimDbException as e:
        abort(400, str(e))

    return ok_response()


@edit_page.route("/markTranslated/<int:doc_id>", methods=["POST"])
def mark_translated_route(doc_id):
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    for p in d.document_as_current_user.get_paragraphs():
        if p.is_translation() and not p.is_setting():
            old_rt = p.get_attr('rt')
            mark_as_translated(p)
            if old_rt != p.get_attr('rt'):
                p.save()
    return ok_response()


@dataclass
class DrawIODataModel:
    data: str
    par_id: str
    doc_id: int


@edit_page.route("/jsframe/drawIOData", methods=['PUT'])
@use_model(DrawIODataModel)
def set_drawio_base(args: DrawIODataModel):
    data, par_id, doc_id = args.data, args.par_id, args.doc_id
    doc = get_doc_or_abort(doc_id)
    verify_edit_access(doc)
    try:
        par = doc.document_as_current_user.get_paragraph(par_id)
    except TimDbException as e:
        return abort(404, str(e))
    plug = Plugin.from_paragraph(par, default_view_ctx)
    if plug.type != 'csPlugin' or plug.values.get('type', '') != 'drawio':
        return abort(400, "Invalid target")
    plug.values['data'] = data
    save_plugin(plug, max_attr_width=float("inf"))
    return ok_response()
