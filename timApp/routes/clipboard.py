"""Routes for the clipboard"""

from .common import *
from documentmodel.clipboard import Clipboard
from flask import Blueprint
from routes.edit import par_response

clipboard = Blueprint('clipboard',
                      __name__,
                      url_prefix='')  # TODO: Better URL prefix.


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

    (par_before, as_ref) = verify_json_params('par_before', 'as_ref', require=False)

    timdb = getTimDb()
    doc = Document(doc_id)
    clip = Clipboard(timdb.files_root_path).get(getCurrentUserId())
    pars = clip.paste_before(doc, par_before, as_ref)

    return par_response(pars, doc)


@clipboard.route('/clipboard', methods=['GET'])
def show_clipboard():
    verifyLoggedIn()
    timdb = getTimDb()
    clip = Clipboard(timdb.files_root_path).get(getCurrentUserId())
    return jsonResponse(clip.read())

