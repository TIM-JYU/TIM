"""Common functions for use with routes."""

from timdb.timdb2 import TimDb
from flask import current_app, session, abort, g, Response, request
import json
from timdb.timdbbase import DocIdentifier
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
    groups = timdb.users.getUserGroups(getCurrentUserId())
    if (len(groups) == 0):
       print("No group found for user id {} ({})!".format(getCurrentUserId(), getCurrentUserName))
       return 0

    return groups[0]['id']
    #return timdb.users.getUserGroups(getCurrentUserId())[0]['id']


def getTimDb():
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

def jsonResponse(jsondata, status_code=200):
    response = Response(json.dumps(jsondata, separators=(',', ':')), mimetype='application/json')
    response.status_code = status_code
    return response

def getNewest(docId):
    docId = int(docId)
    timdb = getTimDb()
    version = timdb.documents.getNewestVersionHash(docId)
    return DocIdentifier(docId, version)

def verify_document_version(doc_id, version):
    timdb = getTimDb()
    #newestVersion = timdb.documents.getDocumentVersions(doc_id, 1)[0]['hash']
    newestVersion = timdb.documents.getNewestVersionHash(doc_id)
    if newestVersion != version:
        abort(400, 'The document version you edited is no longer the latest version. '
                   'Please refresh the page and try again.')


def verify_json_params(*args):
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
