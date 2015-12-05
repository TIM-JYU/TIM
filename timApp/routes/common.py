"""Common functions for use with routes."""
from collections import defaultdict
from documentmodel.docparagraphencoder import DocParagraphEncoder
from documentmodel.document import Document
import pluginControl

from timdb.timdb2 import TimDb
from flask import current_app, session, abort, g, Response, request
import json
from timdb.docidentifier import DocIdentifier
from werkzeug.exceptions import default_exceptions, HTTPException
from flask import make_response, abort as flask_abort
import time

def getCurrentUserId():
    uid = session.get('user_id')
    return uid if uid is not None else 0


def getCurrentUserName():
    name = session.get('user_name')
    return name if name is not None else 'Anonymous'


def getCurrentUserGroup():
    timdb = getTimDb()
    return timdb.users.getPersonalUserGroup(getCurrentUserId())


def getTimDb():
    """

    :rtype : TimDb
    """
    if not hasattr(g, 'timdb'):
        g.timdb = TimDb(db_path=current_app.config['DATABASE'],
                        files_root_path=current_app.config['FILES_PATH'],
                        current_user_name=getCurrentUserName())
    return g.timdb


def verify_edit_access(block_id, message="Sorry, you don't have permission to edit this resource."):
    timdb = getTimDb()
    if not timdb.users.has_edit_access(getCurrentUserId(), block_id):
        abort(403, message)


def has_edit_access(block_id):
    timdb = getTimDb()
    return timdb.users.has_edit_access(getCurrentUserId(), block_id)


def verify_view_access(block_id, require=True):
    timdb = getTimDb()
    if not timdb.users.has_view_access(getCurrentUserId(), block_id):
        if not require:
            return False
        abort(403, "Sorry, you don't have permission to view this resource.")
    return True


def verify_teacher_access(block_id, require=True):
    timdb = getTimDb()
    if not timdb.users.has_teacher_access(getCurrentUserId(), block_id):
        if not require:
            return False
        abort(403, "Sorry, you don't have permission to view this resource.")
    return True


def has_view_access(block_id):
    timdb = getTimDb()
    return timdb.users.has_view_access(getCurrentUserId(), block_id)


def has_comment_right(doc_id):
    return has_view_access(doc_id) and logged_in()


def verify_comment_right(doc_id):
    if not has_comment_right(doc_id):
        abort(403)


def has_read_marking_right(doc_id):
    return has_view_access(doc_id) and logged_in()


def verify_read_marking_right(doc_id):
    if not has_read_marking_right(doc_id):
        abort(403)


def get_rights(doc_id):
    return {'editable': has_edit_access(doc_id),
            'can_mark_as_read': has_read_marking_right(doc_id),
            'can_comment': has_comment_right(doc_id),
            'browse_own_answers': logged_in()
            }


def verifyLoggedIn():
    if not logged_in():
        abort(403, "You have to be logged in to perform this action.")


def has_ownership(block_id):
    timdb = getTimDb()
    return timdb.users.userIsOwner(getCurrentUserId(), block_id)


def verify_ownership(block_id):
    timdb = getTimDb()
    if not timdb.users.userIsOwner(getCurrentUserId(), block_id):
        abort(403, "Sorry, you don't have permission to view this resource.")


def logged_in():
    return getCurrentUserId() != 0


def canWriteToFolder(folderName):
    timdb = getTimDb()
    userFolder = "users/" + getCurrentUserName()
    folder = folderName
    while folder != '':
        if folder == userFolder:
            return True

        folderId = timdb.folders.get_folder_id(folder)
        if folderId is not None:
            return has_edit_access(folderId)

        folder, _ = timdb.folders.split_location(folder)

    return timdb.users.has_admin_access(getCurrentUserId())


def jsonResponse(jsondata, status_code=200):
    response = Response(json.dumps(jsondata,
                                   separators=(',', ':'),
                                   cls=DocParagraphEncoder), mimetype='application/json')
    response.status_code = status_code
    return response


def okJsonResponse():
    return jsonResponse({'status': 'ok'})


def get_newest_document(doc_id):
    """
    Returns the newest Document object with the specified numeric id.

    :rtype: Document
    :type doc_id: int
    :param doc_id: The numeric id.
    :return: The Document object.
    """

    return Document(doc_id, modifier_group_id=getCurrentUserGroup())

def verify_document_version(doc_id, version):
    timdb = getTimDb()
    latest = Document(doc_id).get_version()
    if version != latest:
        abort(400, 'The document version you edited is no longer the latest version. '
                   'Please refresh the page and try again.')


def verify_json_params(*args, require=True, default=None):
    """

    :type args: list[str]
    :rtype: tuple[str]
    """
    result = ()
    json_params = request.get_json()
    for arg in args:
        if arg in json_params:
            val = json_params[arg]
        elif not require:
            val = default
        else:
            abort(400, 'Missing required parameter in request: {}'.format(arg))
            return ()

        result += (val,)
    return result


def unpack_args(*args, types):
    result = ()
    json_params = request.args
    for idx, arg in enumerate(args):
        if arg not in json_params:
            abort(400, 'Missing required parameter in request: {}'.format(arg))
        result = result + (types[idx](json_params[arg]),)
    return result


def hide_names_in_teacher(doc_id):
    return False


def post_process_pars(doc, pars, user_id, sanitize=True, do_lazy=False, edit_window=False):
    timdb = getTimDb()
    current_user = timdb.users.getUser(user_id)
    html_pars, js_paths, css_paths, modules = pluginControl.pluginify(doc,
                                                                      pars,
                                                                      current_user['name'],
                                                                      timdb.answers,
                                                                      user_id,
                                                                      sanitize=sanitize,
                                                                      do_lazy=do_lazy,
                                                                      edit_window=edit_window)
    #req_json = request.get_json()

    #if req_json is not None and 'ref-id' in req_json and req_json['ref-id'] != '':
    #    ref_doc_id = req_json.get('ref-doc-id')
    #    ref_id = req_json.get('ref-id')
    #    html_pars = [par for par in html_pars if par['doc_id'] == ref_doc_id and par['id'] == ref_id]

    if edit_window:
        # Skip readings and notes
        return html_pars, js_paths, css_paths, modules

    # There can be several references of the same paragraph in the document, which is why we need a dict of lists
    pars_dict = defaultdict(list)
    for htmlpar in html_pars:
        if htmlpar.get('ref_id') and htmlpar.get('ref_doc_id'):
            key = htmlpar.get('ref_id'), htmlpar.get('ref_doc_id')
            pars_dict[key].append(htmlpar)

        key = htmlpar['id'], htmlpar['doc_id']
        pars_dict[key].append(htmlpar)

    for p in html_pars:
        p['status'] = ''
        p['notes'] = []

    group = timdb.users.getPersonalUserGroup(user_id)
    readings = timdb.readings.getReadings(group, doc)

    for r in readings:
        key = (r['par_id'], r['doc_id'])
        pars = pars_dict.get(key)
        if pars:
            for p in pars:
                p['status'] = 'read' if r['par_hash'] == p['t'] or r['par_hash'] == p.get('ref_t') else 'modified'
    
    notes = timdb.notes.getNotes(group, doc)

    for n in notes:
        key = (n['par_id'], n['doc_id'])
        pars = pars_dict.get(key)
        if pars:
            n['editable'] = n['UserGroup_id'] == group or timdb.users.userIsOwner(getCurrentUserId(), doc.doc_id)
            n.pop('UserGroup_id', None)
            n['private'] = n['access'] == 'justme'
            for p in pars:
                if 'notes' not in p:
                    p['notes'] = []
                p['notes'].append(n)
    return html_pars, js_paths, css_paths, modules


def get_referenced_pars_from_req(par):
    if par.is_reference():
        return [ref_par for ref_par in par.get_referenced_pars(set_html=False, tr_get_one=False)]
    else:
        return [par]
