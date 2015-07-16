"""Routes for manage view."""
from flask import Blueprint, render_template
from .common import *
from timdb.docidentifier import DocIdentifier
from documentmodel.document import Document

manage_page = Blueprint('manage_page',
                        __name__,
                        url_prefix='')  # TODO: Better URL prefix.

@manage_page.route("/manage/<int:doc_id>")
def manage(doc_id):
    timdb = getTimDb()
    isFolder = False
    if not timdb.documents.documentExists(doc_id):
        if timdb.folders.folderExists(doc_id):
            isFolder = True
        else:
            abort(404)

    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)

    possible_groups = timdb.users.getUserGroupsPrintable(getCurrentUserId())
    editors = timdb.users.getEditors(doc_id)
    viewers = timdb.users.getViewers(doc_id)

    if isFolder:
        doc_data = timdb.folders.getFolder(doc_id)
        doc_data['versions'] = []
        doc_data['fulltext'] = ''
    else:
        doc = Document(doc_id)
        doc_data = timdb.documents.getDocument(doc_id)
        doc_data['versions'] = [entry for entry in doc.get_changelog()]
        doc_data['fulltext'] = doc.export_markdown()
        for ver in doc_data['versions']:
            ver['group'] = timdb.users.get_user_group_name(ver.pop('group_id'))

    doc_data['owner'] = timdb.users.getOwnerGroup(doc_id)
    return render_template('manage.html',
                           objName='folder' if isFolder else 'document',
                           objNameC='Folder' if isFolder else 'Document',
                           doc=doc_data,
                           editors=editors,
                           viewers=viewers,
                           user_groups=possible_groups,
                           isFolder=str(isFolder).lower())

@manage_page.route("/changeOwner/<int:doc_id>/<int:new_owner>", methods=["PUT"])
def changeOwner(doc_id, new_owner):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id) and not timdb.folders.folderExists(doc_id):
        abort(404)
    verifyOwnership(doc_id)
    possible_groups = timdb.users.getUserGroups(getCurrentUserId())
    if new_owner not in [group['id'] for group in possible_groups]:
        abort(403, "You must belong to the new usergroup.")
    timdb.documents.setOwner(doc_id, new_owner)
    return "Success"

@manage_page.route("/addPermission/<int:doc_id>/<group_name>/<perm_type>", methods=["PUT"])
def addPermission(doc_id, group_name, perm_type):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id) and not timdb.folders.folderExists(doc_id):
        abort(404, 'This document does not exist.')
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403, "You don't have permission to add permissions to this document.")

    groups = timdb.users.getUserGroupsByName(group_name)
    if len(groups) == 0:
        abort(404, 'No user group with this name was found.')

    group_id = groups[0]['id']

    if perm_type == 'edit':
        timdb.users.grantEditAccess(group_id, doc_id)
    elif perm_type == 'view':
        timdb.users.grantViewAccess(group_id, doc_id)
    else:
        abort(400, 'Invalid permission type.')
    return "Success"

@manage_page.route("/removePermission/<int:doc_id>/<int:group_id>/<perm_type>", methods=["PUT"])
def removePermission(doc_id, group_id, perm_type):
    timdb = getTimDb()
    if not timdb.documents.documentExists(doc_id) and not timdb.folders.folderExists(doc_id):
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
    isFolder = False

    while new_name.startswith('/'):
        new_name = new_name[1:]

    if new_name.endswith('/'):
        return jsonResponse({'message': 'The document name cannot end with /'}, 404)

    if timdb.documents.get_document_id(new_name) is not None or timdb.folders.getFolderId(new_name) is not None:
        return jsonResponse({'message': 'Item with a same name already exists.'}, 403)

    if not timdb.documents.documentExists(doc_id):
        if timdb.folders.folderExists(doc_id):
            isFolder = True
        else:
            return jsonResponse({'message': 'The document or folder does not exist!'}, 404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to rename this object."}, 403)

    if isFolder:
        old_name = timdb.folders.getFolder(doc_id)['name']
    else:
        old_name = timdb.documents.getDocument(doc_id)['name']

    userName = getCurrentUserName()

    if not timdb.users.isUserInGroup(userName, 'Administrators'):
        if not canWriteToFolder(timdb.folders.getContainingFolderName(new_name)):
            return jsonResponse({'message': "You don't have permission to write to that folder."}, 403)

    if isFolder:
        timdb.folders.renameFolder(doc_id, new_name)
    else:
        timdb.documents.renameDocument(DocIdentifier(doc_id, ''), new_name)

    return "Success"

@manage_page.route("/getPermissions/<int:doc_id>")
def getPermissions(doc_id):
    timdb = getTimDb()
    isFolder = False
    if not timdb.documents.documentExists(doc_id):
        if timdb.folders.folderExists(doc_id):
            isFolder = True
        else:
            abort(404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        abort(403)

    doc_data = timdb.folders.getFolder(doc_id) if isFolder else timdb.documents.getDocument(doc_id)
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

@manage_page.route("/folders/<int:doc_id>", methods=["DELETE"])
def deleteFolder(doc_id):
    timdb = getTimDb()
    if not timdb.folders.folderExists(doc_id):
        return jsonResponse({'message': 'Folder does not exist.'}, 404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to delete this folder."}, 403)
    if not timdb.folders.isEmpty(doc_id):
        return jsonResponse({'message': "The folder is not empty. Only empty folders can be deleted."}, 403)

    timdb.folders.deleteFolder(doc_id)
    return "Success"

