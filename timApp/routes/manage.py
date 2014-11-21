"""Routes for manage view."""
import os
import cssutils
from flask import Blueprint, render_template, request
from .common import *
from timdb.timdbbase import DocIdentifier

manage_page = Blueprint('manage_page',
                        __name__,
                        url_prefix='')  # TODO: Better URL prefix.

@manage_page.route("/manage/<int:doc_id>")
def manage(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    doc_data = timdb.documents.getDocument(doc_id)
    doc_data['versions'] = timdb.documents.getDocumentVersions(doc_id)
    hash = doc_data['versions'][0]['hash']
    doc_data['owner'] = timdb.users.getOwnerGroup(doc_id)
    doc_data['fulltext'] = timdb.documents.getDocumentMarkdown(DocIdentifier(doc_id, hash))
    editors = timdb.users.getEditors(doc_id)
    viewers = timdb.users.getViewers(doc_id)
    return render_template('manage.html', doc=doc_data, editors=editors, viewers=viewers)

@manage_page.route("/addPermission/<int:doc_id>/<group_name>/<perm_type>", methods=["PUT"])
def addPermission(doc_id, group_name, perm_type):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)

    groups = timdb.users.getUserGroupsByName(group_name)
    if len(groups) == 0:
        return jsonResponse({'message' : 'No user group with this name was found.'}, 404)

    group_id = groups[0]['id']

    if perm_type == 'edit':
        timdb.users.grantEditAccess(group_id, doc_id)
    elif perm_type == 'view':
        timdb.users.grantViewAccess(group_id, doc_id)
    else:
        abort(400)
    return "Success"

@manage_page.route("/removePermission/<int:doc_id>/<int:group_id>/<perm_type>", methods=["PUT"])
def removePermission(doc_id, group_id, perm_type):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    if perm_type == 'edit':
        timdb.users.removeEditAccess(group_id, doc_id)
    elif perm_type == 'view':
        timdb.users.removeViewAccess(group_id, doc_id)
    else:
        abort(400)
    return "Success"

@manage_page.route("/rename/<int:doc_id>", methods=["PUT"])
def renameDocument(doc_id):
    timdb = getTimDb()
    new_name = request.get_json()['new_name']
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    timdb.documents.renameDocument(DocIdentifier(doc_id, ''), new_name)
    return "Success"

@manage_page.route("/getPermissions/<int:doc_id>")
def getPermissions(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id):
        abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)
    doc_data = timdb.documents.getDocument(doc_id)
    editors = timdb.users.getEditors(doc_id)
    viewers = timdb.users.getViewers(doc_id)
    return jsonResponse({'doc' : doc_data, 'editors' : editors, 'viewers' : viewers})

@manage_page.route("/documents/<int:doc_id>", methods=["DELETE"])
def deleteDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id):
        return jsonResponse({'message': 'Document does not exist.'}, 404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to delete this document."}, 403)
    timdb.documents.deleteDocument(doc_id)
    return "Success"