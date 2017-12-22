"""Routes for the clipboard."""

from flask import Blueprint, request
from flask import abort
from flask import current_app
from flask import g

from timApp.accesshelper import verify_logged_in, get_doc_or_abort
from timApp.accesshelper import verify_view_access, verify_edit_access
from timApp.dbaccess import get_timdb
from timApp.documentmodel.clipboard import Clipboard
from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import Document
from timApp.documentmodel.documenteditresult import DocumentEditResult
from timApp.requesthelper import verify_json_params, get_option
from timApp.responsehelper import json_response, ok_response
from timApp.routes.edit import par_response
from timApp.sessioninfo import get_current_user_id
from timApp.synchronize_translations import synchronize_translations
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.readings import copy_readings
from timApp.timdb.tim_models import db

clipboard = Blueprint('clipboard',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@clipboard.url_value_preprocessor
def pull_doc_id(endpoint, values):
    if current_app.url_map.is_endpoint_expecting(endpoint, 'doc_id'):
        doc_id = values['doc_id']
        if doc_id is None:
            abort(400)
        g.doc_id = doc_id
        g.docentry = get_doc_or_abort(doc_id)
        if not g.docentry:
            abort(404)


@clipboard.route('/clipboard/cut/<int:doc_id>/<from_par>/<to_par>', methods=['POST'])
def cut_to_clipboard(doc_id, from_par, to_par):
    verify_logged_in()
    verify_edit_access(g.docentry)

    (area_name,) = verify_json_params('area_name', require=False)

    timdb = get_timdb()
    doc = g.docentry.document_as_current_user
    clip = Clipboard(timdb.files_root_path).get(get_current_user_id())
    try:
        pars = clip.cut_pars(doc, from_par, to_par, area_name)
    except TimDbException as e:
        return abort(400, str(e))
    g.docentry.update_last_modified()
    db.session.commit()
    synchronize_translations(g.docentry, DocumentEditResult(deleted=pars))

    return json_response({'doc_ver': doc.get_version(), 'pars': [{'id': p.get_id()} for p in pars]})


@clipboard.route('/clipboard/copy/<int:doc_id>/<from_par>/<to_par>', methods=['POST'])
def copy_to_clipboard(doc_id, from_par, to_par):
    verify_logged_in()
    verify_view_access(g.docentry)

    (area_name, ref_doc_id) = verify_json_params('area_name', 'ref_doc_id', require=False)
    ref_doc = Document(ref_doc_id) if ref_doc_id is not None and ref_doc_id != doc_id else None

    timdb = get_timdb()
    doc = g.docentry.document_as_current_user
    clip = Clipboard(timdb.files_root_path).get(get_current_user_id())
    try:
        clip.copy_pars(doc, from_par, to_par, area_name, ref_doc)
    except TimDbException as e:
        return abort(400, str(e))

    return ok_response()


@clipboard.route('/clipboard/paste/<int:doc_id>', methods=['POST'])
def paste_from_clipboard(doc_id):
    verify_logged_in()
    verify_edit_access(g.docentry)

    (par_before, par_after, as_ref) = verify_json_params('par_before', 'par_after', 'as_ref', require=False, default='')

    timdb = get_timdb()
    doc = g.docentry.document_as_current_user
    clip = Clipboard(timdb.files_root_path).get(get_current_user_id())
    meta = clip.read_metadata()
    was_cut = meta.get('last_action') == 'cut'

    if meta.get('empty', True):
        abort(400, 'The clipboard is empty.')
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
                copy_readings(src_par, dest_par)
                if was_cut:
                    timdb.notes.move_notes(src_par, dest_par, commit=False)

        except ValueError:
            pass

    timdb.commit()
    edit_result = DocumentEditResult(added=pars)
    synchronize_translations(g.docentry, edit_result)
    return par_response(pars, doc, edit_result=edit_result)


# TODO unused route?
@clipboard.route('/clipboard/deletesrc/<int:doc_id>', methods=['POST'])
def delete_from_source(doc_id):
    verify_logged_in()
    verify_edit_access(g.docentry)

    timdb = get_timdb()
    doc = g.docentry.document_as_current_user
    clip = Clipboard(timdb.files_root_path).get(get_current_user_id())
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
    timdb = get_timdb()

    doc_id = get_option(request, 'doc_id', default=None, cast=int)
    if doc_id is None:
        return abort(400, 'doc_id missing')
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    doc = d.document

    clip = Clipboard(timdb.files_root_path).get(get_current_user_id())
    pars = [DocParagraph.from_dict(doc, par) for par in clip.read() or []]
    return par_response(pars, doc)


@clipboard.route('/clipboardstatus', methods=['GET'])
def get_clipboard_status():
    verify_logged_in()
    timdb = get_timdb()
    clip = Clipboard(timdb.files_root_path).get(get_current_user_id())
    status = clip.read_metadata()
    return json_response(status)
