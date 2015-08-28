"""Common functions for use with routes."""
from documentmodel.docparagraphencoder import DocParagraphEncoder
from documentmodel.document import Document
import pluginControl

from timdb.timdb2 import TimDb
from flask import current_app, session, abort, g, Response, request
import json
from timdb.docidentifier import DocIdentifier
from werkzeug.exceptions import default_exceptions, HTTPException
from flask import make_response, abort as flask_abort, request
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

def verifyEditAccess(block_id, message="Sorry, you don't have permission to edit this resource."):
    timdb = getTimDb()
    if not timdb.users.userHasEditAccess(getCurrentUserId(), block_id):
        abort(403, message)

def hasEditAccess(block_id):
    timdb = getTimDb()
    return timdb.users.userHasEditAccess(getCurrentUserId(), block_id)

def verifyViewAccess(block_id, require=True):
    timdb = getTimDb()
    if not timdb.users.userHasViewAccess(getCurrentUserId(), block_id):
        if not require:
            return False
        abort(403, "Sorry, you don't have permission to view this resource.")
    return True

def hasViewAccess(block_id):
    timdb = getTimDb()
    return timdb.users.userHasViewAccess(getCurrentUserId(), block_id)

def hasCommentRight(doc_id):
    return hasViewAccess(doc_id) and loggedIn()

def verifyCommentRight(doc_id):
    if not hasCommentRight(doc_id):
        abort(403)

def hasReadMarkingRight(doc_id):
    return hasViewAccess(doc_id) and loggedIn()

def verifyReadMarkingRight(doc_id):
    if not hasReadMarkingRight(doc_id):
        abort(403)


def get_rights(doc_id):
    return {'editable': hasEditAccess(doc_id),
            'can_mark_as_read': hasReadMarkingRight(doc_id),
            'can_comment': hasCommentRight(doc_id),
            'browse_own_answers': loggedIn()
            }


def verifyLoggedIn():
    if not loggedIn():
        abort(403, "You have to be logged in to perform this action.")

def hasOwnership(block_id):
    timdb = getTimDb()
    return timdb.users.userIsOwner(getCurrentUserId(), block_id)

def verifyOwnership(block_id):
    timdb = getTimDb()
    if not timdb.users.userIsOwner(getCurrentUserId(), block_id):
        abort(403, "Sorry, you don't have permission to view this resource.")

def loggedIn():
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
            return hasEditAccess(folderId)

        folder, _ = timdb.folders.split_location(folder)

    return timdb.users.isUserInGroup(getCurrentUserName(), 'Administrators')


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


def verify_json_params(*args, require=True):
    """

    :type args: list[str]
    :rtype: tuple[str]
    """
    result = ()
    json_params = request.get_json()
    for arg in args:
        if arg not in json_params:
            if require:
                abort(400, 'Missing required parameter in request: {}'.format(arg))
            else:
                continue
        result = result + (json_params[arg],)
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


def quick_post_process(pars):
    htmlpars = [par.html_dict() for par in pars]
    for par in htmlpars:
        par['status'] = ''
    return htmlpars

def post_process_pars(pars, doc_id, user_id, sanitize=True, do_lazy=False):
    timdb = getTimDb()
    current_user = timdb.users.getUser(user_id)
    group = timdb.users.getPersonalUserGroup(user_id)
    t0 = time.time()
    pars, js_paths, css_paths, modules = pluginControl.pluginify(pars,
                                                                 current_user['name'],
                                                                 timdb.answers,
                                                                 user_id,
                                                                 sanitize=sanitize,
                                                                 do_lazy=do_lazy)
    print("pluginify time: {} s".format(time.time() - t0))
    req_json = request.get_json()
    t0 = time.time()

    if req_json is not None and 'ref-id' in req_json and req_json['ref-id'] != '':
        htmlpars = [par.html_dict() for par in pars
                if str(par.get_doc_id()) == req_json.get('ref-doc-id') and
                par.get_id() == req_json.get('ref-id')]
    else:
        htmlpars = [par.html_dict() for par in pars]

    print("dict1 time: {} s".format(time.time() - t0))
    t0 = time.time()
    pars_dict = dict(((par['id'], par['doc_id']), par) for par in htmlpars)
    print("dict2 time: {} s".format(time.time() - t0))

    t0 = time.time()
    readings = timdb.readings.getReadings(group, Document(doc_id))
    for r in readings:
        key = (r['par_id'], r['doc_id'])
        if key in pars_dict:
            pars_dict[key]['status'] = 'read' if r['par_hash'] == pars_dict[key]['t'] else 'modified'
    print("readings time: {} s".format(time.time() - t0))

    t0 = time.time()
    notes = timdb.notes.getNotes(group, Document(doc_id))
    note_dict = {}
    for n in notes:
        key = (n['par_id'], n['doc_id'])
        if key in pars_dict:
            if 'notes' not in pars_dict[key]:
                pars_dict[key]['notes'] = []

            n['editable'] = n['UserGroup_id'] == group or timdb.users.userIsOwner(getCurrentUserId(), doc_id)
            n.pop('UserGroup_id', None)
            n['private'] = n['access'] == 'justme'
            pars_dict[key]['notes'].append(n)
    print("notes time: {} s".format(time.time() - t0))

    return htmlpars, js_paths, css_paths, modules


def get_referenced_par_from_req(par):
    if par.is_reference():
        refs = par.get_referenced_pars()
        for ref in refs:
            if ref.get_id() == request.get_json().get('ref-id'):
                return ref
        abort(400, 'Invalid reference par id')
    else:
        return par
