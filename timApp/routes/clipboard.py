"""Routes for the clipboard"""

from .common import *
from documentmodel.clipboard import Clipboard
from documentmodel.docparagraph import DocParagraph
from flask import Blueprint
from routes.edit import par_response

clipboard = Blueprint('clipboard',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


@clipboard.route('/clipboard/cut/<int:doc_id>/<from_par>/<to_par>', methods=['POST'])
def cut_to_clipboard(doc_id, from_par, to_par):
    verifyLoggedIn()
    verify_doc_exists(doc_id)
    verify_edit_access(doc_id)

    (area_name,) = verify_json_params('area_name', require=False)

    timdb = getTimDb()
    doc = Document(doc_id)
    clip = Clipboard(timdb.files_root_path).get(getCurrentUserId())
    pars = clip.cut_pars(doc, from_par, to_par, area_name)
    timdb.documents.update_last_modified(doc)

    return jsonResponse({'doc_ver': doc.get_version(), 'pars': [{'id': p.get_id()} for p in pars]})


@clipboard.route('/clipboard/copy/<int:doc_id>/<from_par>/<to_par>', methods=['POST'])
def copy_to_clipboard(doc_id, from_par, to_par):
    verifyLoggedIn()
    verify_doc_exists(doc_id)
    verify_view_access(doc_id)

    (area_name, ref_doc_id) = verify_json_params('area_name', 'ref_doc_id', require=False)
    ref_doc = Document(ref_doc_id) if ref_doc_id is not None and ref_doc_id != doc_id else None

    timdb = getTimDb()
    doc = Document(doc_id)
    clip = Clipboard(timdb.files_root_path).get(getCurrentUserId())
    clip.copy_pars(doc, from_par, to_par, area_name, ref_doc)

    return okJsonResponse()


@clipboard.route('/clipboard/paste/<int:doc_id>', methods=['POST'])
def paste_from_clipboard(doc_id):
    verifyLoggedIn()
    verify_doc_exists(doc_id)
    verify_edit_access(doc_id)

    (par_before, par_after, as_ref) = verify_json_params('par_before', 'par_after', 'as_ref', require=False)

    timdb = getTimDb()
    doc = Document(doc_id)
    clip = Clipboard(timdb.files_root_path).get(getCurrentUserId())

    if clip.read(as_ref=False) is None:
        abort(400, 'The clipboard is empty.')
    if as_ref and clip.read(as_ref=True) is None:
        abort(400, 'The contents of the clipboard cannot be pasted as a reference.')

    if par_before is not None and par_after is None:
        pars = clip.paste_before(doc, par_before, as_ref)
    elif par_before is None and par_after is not None:
        pars = clip.paste_after(doc, par_after, as_ref)
    else:
        abort(400, 'Missing required parameter in request: par_before or par_after (not both)')

    return par_response(pars, doc)


@clipboard.route('/clipboard/deletesrc/<int:doc_id>', methods=['POST'])
def delete_from_source(doc_id):
    verifyLoggedIn()
    verify_doc_exists(doc_id)
    verify_edit_access(doc_id)

    timdb = getTimDb()
    doc = Document(doc_id)
    clip = Clipboard(timdb.files_root_path).get(getCurrentUserId())
    pars = clip.read(as_ref=True)
    if not pars:
        return jsonResponse({'doc_ver': doc.get_version(), 'pars': []})

    my_pars = [{'id': p['attrs']['rp']} for p in pars if p['attrs']['rd'] == str(doc_id)]
    clip.delete_from_source()
    clip.clear()

    return jsonResponse({'doc_ver': doc.get_version(), 'pars': my_pars})


@clipboard.route('/clipboard', methods=['GET'])
def show_clipboard():
    verifyLoggedIn()
    timdb = getTimDb()

    (doc_id,) = verify_json_params('doc_id', require=False, default=None)
    if doc_id is None:
        doc = Document()
    else:
        verify_doc_exists(doc_id)
        verify_view_access(doc_id)
        doc = Document(doc_id)

    clip = Clipboard(timdb.files_root_path).get(getCurrentUserId())
    pars = [DocParagraph.from_dict(doc, par) for par in clip.read() or []]
    return par_response(pars, doc, edit_window=True)

