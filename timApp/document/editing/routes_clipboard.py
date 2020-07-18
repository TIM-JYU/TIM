"""Routes for the clipboard."""
from dataclasses import dataclass
from flask import Blueprint, request
from flask import abort
from flask import current_app
from flask import g

from timApp.auth.accesshelper import verify_logged_in, get_doc_or_abort
from timApp.auth.accesshelper import verify_view_access, verify_edit_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document, par_list_to_text
from timApp.document.editing.clipboard import Clipboard
from timApp.document.editing.documenteditresult import DocumentEditResult
from timApp.document.editing.routes import par_response, verify_par_edit_access
from timApp.document.translation.synchronize_translations import synchronize_translations
from timApp.note.notes import move_notes
from timApp.notification.notification import NotificationType
from timApp.notification.notify import notify_doc_watchers
from timApp.readmark.readings import copy_readings
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import verify_json_params, get_option
from timApp.util.flask.responsehelper import json_response, ok_response

clipboard = Blueprint('clipboard',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@dataclass
class WithDocData:
    doc_id: int
    docentry: DocInfo


wd: WithDocData = g


@clipboard.url_value_preprocessor
def pull_doc_id(endpoint, values):
    if current_app.url_map.is_endpoint_expecting(endpoint, 'doc_id'):
        doc_id = values['doc_id']
        if doc_id is None:
            abort(400)
        wd.doc_id = doc_id
        wd.docentry = get_doc_or_abort(doc_id)
        if not wd.docentry:
            abort(404)


@clipboard.route('/clipboard/cut/<int:doc_id>/<from_par>/<to_par>', methods=['POST'])
def cut_to_clipboard(doc_id, from_par, to_par):
    verify_logged_in()
    verify_edit_access(wd.docentry)

    (area_name,) = verify_json_params('area_name', require=False)

    doc: Document = wd.docentry.document_as_current_user
    version_before = doc.get_version()
    clip = Clipboard().get(get_current_user_object())
    try:
        for p in doc.get_section(from_par, to_par):
            verify_par_edit_access(p)
        pars = clip.cut_pars(doc, from_par, to_par, area_name)
    except TimDbException as e:
        return abort(400, str(e))
    wd.docentry.update_last_modified()
    db.session.commit()
    synchronize_translations(wd.docentry, DocumentEditResult(deleted=pars))
    notify_doc_watchers(wd.docentry, par_list_to_text(pars), NotificationType.ParDeleted, old_version=version_before)
    db.session.commit()
    return json_response({'doc_ver': doc.get_version(), 'pars': [{'id': p.get_id()} for p in pars]})


@clipboard.route('/clipboard/copy/<int:doc_id>/<from_par>/<to_par>', methods=['POST'])
def copy_to_clipboard(doc_id, from_par, to_par):
    verify_logged_in()
    verify_view_access(wd.docentry)

    (area_name, ref_doc_id) = verify_json_params('area_name', 'ref_doc_id', require=False)
    ref_doc = Document(ref_doc_id) if ref_doc_id is not None and ref_doc_id != doc_id else None

    doc = wd.docentry.document_as_current_user
    clip = Clipboard().get(get_current_user_object())
    try:
        clip.copy_pars(doc, from_par, to_par, area_name, ref_doc, disable_ref=False)
    except TimDbException as e:
        return abort(400, str(e))

    return ok_response()


@clipboard.route('/clipboard/paste/<int:doc_id>', methods=['POST'])
def paste_from_clipboard(doc_id):
    verify_logged_in()
    verify_edit_access(wd.docentry)

    (par_before, par_after, as_ref) = verify_json_params('par_before', 'par_after', 'as_ref', require=False, default='')

    doc = wd.docentry.document_as_current_user
    version_before = doc.get_version()
    clip = Clipboard().get(get_current_user_object())
    meta = clip.read_metadata()
    was_cut = meta.get('last_action') == 'cut'

    if meta.get('empty', True):
        abort(400, 'The clipboard is empty.')
    if not as_ref and meta.get('disable_content'):
        abort(403, 'The contents of the clipboard cannot be pasted as content.')
    if as_ref and meta.get('disable_ref'):
        abort(400, 'The contents of the clipboard cannot be pasted as a reference.')

    try:
        if par_before and not par_after:
            pars = clip.paste_before(doc, par_before, as_ref)
        elif not par_before and par_after:
            pars = clip.paste_after(doc, par_after, as_ref)
        else:
            return abort(400, 'Missing required parameter in request: par_before or par_after (not both)')
    except TimDbException as e:
        return abort(400, str(e))

    src_doc = None
    parrefs = clip.read(as_ref=True, force_parrefs=True)
    for (src_par_dict, dest_par) in zip(parrefs, pars):
        try:
            src_docid = int(src_par_dict['attrs']['rd'])
            src_parid = src_par_dict['attrs']['rp']
            par_id = dest_par.get_id()
            if (doc_id, par_id) != (src_docid, src_parid):
                if src_doc is None or str(src_doc.doc_id) != str(src_docid):
                    src_doc = Document(src_docid)
                src_par = DocParagraph.get_latest(src_doc, src_parid)
                # TODO: Figure out a way to copy readings safely without exposing them
                if was_cut:
                    copy_readings(src_par, dest_par)
                    move_notes(src_par, dest_par)

        except ValueError:
            pass

    edit_result = DocumentEditResult(added=pars)
    synchronize_translations(wd.docentry, edit_result)
    notify_doc_watchers(
        wd.docentry,
        par_list_to_text(pars),
        NotificationType.ParAdded,
        old_version=version_before,
        par=pars[0],
    )
    return par_response(pars, wd.docentry, edit_result=edit_result)


# TODO unused route?
@clipboard.route('/clipboard/deletesrc/<int:doc_id>', methods=['POST'])
def delete_from_source(doc_id):
    verify_logged_in()
    verify_edit_access(wd.docentry)

    doc = wd.docentry.document_as_current_user
    clip = Clipboard().get(get_current_user_object())
    pars = clip.read(as_ref=True, force_parrefs=True)
    if not pars:
        return json_response({'doc_ver': doc.get_version(), 'pars': []})

    my_pars = [{'id': p['attrs']['rp']} for p in pars if p['attrs']['rd'] == str(doc_id)]
    clip.delete_from_source()
    clip.clear()

    return json_response({'doc_ver': doc.get_version(), 'pars': my_pars})


@clipboard.route('/clipboard', methods=['GET'])
def show_clipboard():
    verify_logged_in()

    doc_id = get_option(request, 'doc_id', default=None, cast=int)
    if doc_id is None:
        return abort(400, 'doc_id missing')
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    doc = d.document

    clip = Clipboard().get(get_current_user_object())
    pars = [DocParagraph.from_dict(doc, par) for par in clip.read() or []]
    return par_response(pars, d)


@clipboard.route('/clipboardstatus', methods=['GET'])
def get_clipboard_status():
    verify_logged_in()
    clip = Clipboard().get(get_current_user_object())
    status = clip.read_metadata()
    return json_response(status)
