"""Common functions for use with routes."""
from documentmodel.docparagraphencoder import DocParagraphEncoder
from documentmodel.document import Document

from timdb.timdb2 import TimDb
from flask import current_app, session, abort, g, Response, request
import json
from timdb.docidentifier import DocIdentifier
from werkzeug.exceptions import default_exceptions, HTTPException
from flask import make_response, abort as flask_abort, request

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

def verifyViewAccess(block_id):
    timdb = getTimDb()
    if not timdb.users.userHasViewAccess(getCurrentUserId(), block_id):
        abort(403, "Sorry, you don't have permission to view this resource.")

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
    #newestVersion = timdb.documents.getDocumentVersions(doc_id, 1)[0]['hash']
    newestVersion = timdb.documents.getNewestVersionHash(doc_id)
    if newestVersion != version:
        abort(400, 'The document version you edited is no longer the latest version. '
                   'Please refresh the page and try again.')


def verify_json_params(*args):
    """

    :type args: list[str]
    :rtype: tuple[str]
    """
    result = ()
    json_params = request.get_json()
    for arg in args:
        if arg not in json_params:
            abort(400, 'Missing required parameter in request: {}'.format(arg))
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
