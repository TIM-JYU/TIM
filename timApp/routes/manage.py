"""Routes for manage view."""
from flask import Blueprint, render_template

from .common import *

manage_page = Blueprint('manage_page',
                        __name__,
                        url_prefix='')  # TODO: Better URL prefix.


@manage_page.route("/manage/<path:path>")
def manage(path):
    timdb = getTimDb()
    isFolder = False
    doc_id, doc_name = timdb.documents.resolve_doc_id_name(path)
    if doc_id is None:
        try:
            folder_id = int(path)
            if not timdb.folders.exists(folder_id):
                abort(404)
        except ValueError:
            folder_id = timdb.folders.get_folder_id(path.rstrip('/'))
            if folder_id is None:
                abort(404)
        isFolder = True
        block_id = folder_id
    else:
        block_id = doc_id

    if not timdb.users.has_manage_access(getCurrentUserId(), block_id):
        abort(403)

    possible_groups = timdb.users.getUserGroupsPrintable(getCurrentUserId())
    grouprights = timdb.users.get_rights_holders(block_id)
    access_types = timdb.users.get_access_types()

    if isFolder:
        doc_data = timdb.folders.get(block_id)
        doc_data['versions'] = []
        doc_data['fulltext'] = ''
    else:
        doc = Document(block_id)
        doc_data = {'id': block_id}
        doc_name = timdb.documents.get_first_document_name(block_id)
        if doc_name is not None:
            doc_data['shortname'] = timdb.documents.get_short_name(doc_name)
            doc_data['name'] = doc_name
            doc_data['fullname'] = doc_name
        else:
            doc_data['shortname'] = None
            doc_data['fullname'] = None

        doc_data['versions'] = [entry for entry in doc.get_changelog()]
        doc_data['fulltext'] = doc.export_markdown()
        for ver in doc_data['versions']:
            ver['group'] = timdb.users.get_user_group_name(ver.pop('group_id'))

    doc_data['owner'] = timdb.users.getOwnerGroup(block_id)
    return render_template('manage.html',
                           route="manage",
                           translations=timdb.documents.get_translations(doc_id),
                           objName='folder' if isFolder else 'document',
                           objNameC='Folder' if isFolder else 'Document',
                           doc=doc_data,
                           grouprights=grouprights,
                           access_types=access_types,
                           user_groups=possible_groups,
                           folder=isFolder,
                           rights=get_rights(block_id))


@manage_page.route("/changeOwner/<int:doc_id>/<int:new_owner>", methods=["PUT"])
def changeOwner(doc_id, new_owner):
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id) and not timdb.folders.exists(doc_id):
        abort(404)
    verify_ownership(doc_id)
    possible_groups = timdb.users.getUserGroups(getCurrentUserId())
    if new_owner not in [group['id'] for group in possible_groups]:
        abort(403, "You must belong to the new usergroup.")
    timdb.documents.setOwner(doc_id, new_owner)
    return "Success"


@manage_page.route("/addPermission/<int:doc_id>/<group_name>/<perm_type>", methods=["PUT"])
def addPermission(doc_id, group_name, perm_type):
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id) and not timdb.folders.exists(doc_id):
        abort(404, 'This document does not exist.')
    if not timdb.users.has_manage_access(getCurrentUserId(), doc_id):
        abort(403, "You don't have permission to add permissions to this document.")

    groups = timdb.users.getUserGroupsByName(group_name)
    if len(groups) == 0:
        abort(404, 'No user group with this name was found.')

    group_id = groups[0]['id']

    try:
        timdb.users.grant_access(group_id, doc_id, perm_type)
    except KeyError:
        abort(400, 'Invalid permission type.')
    return "Success"


@manage_page.route("/removePermission/<int:doc_id>/<int:group_id>/<perm_type>", methods=["PUT"])
def removePermission(doc_id, group_id, perm_type):
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id) and not timdb.folders.exists(doc_id):
        abort(404)
    if not timdb.users.has_manage_access(getCurrentUserId(), doc_id):
        abort(403)
    try:
        timdb.users.remove_access(group_id, doc_id, perm_type)
    except KeyError:
        abort(400, 'Unknown permission type')
    return "Success"


@manage_page.route("/alias/<int:doc_id>/", methods=["GET"])
def getDocNames(doc_id):
    timdb = getTimDb()
    names = timdb.documents.get_names(doc_id, include_nonpublic=True)
    return jsonResponse(names)


@manage_page.route("/alias/<int:doc_id>/<path:new_alias>", methods=["PUT"])
def add_alias(doc_id, new_alias):
    timdb = getTimDb()
    is_public = bool(request.get_json()['public'])

    new_alias = new_alias.strip('/')

    if not timdb.documents.exists(doc_id):
        return jsonResponse({'message': 'The document does not exist!'}, 404)

    if not timdb.users.has_manage_access(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to rename this object."}, 403)

    if timdb.documents.get_document_id(new_alias) is not None or timdb.folders.get_folder_id(new_alias) is not None:
        return jsonResponse({'message': 'Item with a same name already exists.'}, 403)

    parent_folder, _ = timdb.folders.split_location(new_alias)

    if not canWriteToFolder(parent_folder):
        return jsonResponse({'message': "You don't have permission to write to that folder."}, 403)

    timdb.folders.create(parent_folder, getCurrentUserGroup())
    timdb.documents.add_name(doc_id, new_alias, is_public)
    return "Success"


@manage_page.route("/alias/<int:doc_id>/<path:alias>", methods=["POST"])
def change_alias(doc_id, alias):
    timdb = getTimDb()
    alias = alias.strip('/')
    new_alias = request.get_json()['new_name'].strip('/')
    is_public = bool(request.get_json()['public'])

    doc_id2 = timdb.documents.get_document_id(alias)
    if doc_id2 is None:
        return jsonResponse({'message': 'The document does not exist!'}, 404)
    if doc_id2 != doc_id:
        return jsonResponse({'message': 'The document name does not match the id!'}, 404)

    if not timdb.users.has_manage_access(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to rename this object."}, 403)

    new_parent, _ = timdb.folders.split_location(new_alias)

    if alias != new_alias:
        if timdb.documents.get_document_id(new_alias) is not None or timdb.folders.get_folder_id(new_alias) is not None:
            return jsonResponse({'message': 'Item with a same name already exists.'}, 403)
        parent, _ = timdb.folders.split_location(alias)
        if not canWriteToFolder(parent):
            return jsonResponse({'message': "You don't have permission to write to the source folder."}, 403)

    if not canWriteToFolder(new_parent):
        return jsonResponse({'message': "You don't have permission to write to the destination folder."}, 403)

    timdb.folders.create(new_parent, getCurrentUserGroup())
    timdb.documents.change_name(doc_id, alias, new_alias, is_public)
    return "Success"


@manage_page.route("/alias/<int:doc_id>/<path:alias>", methods=["DELETE"])
def remove_alias(doc_id, alias):
    timdb = getTimDb()
    alias = alias.strip('/')

    doc_id2 = timdb.documents.get_document_id(alias)
    if doc_id2 is None:
        return jsonResponse({'message': 'The document does not exist!'}, 404)
    if doc_id2 != doc_id:
        return jsonResponse({'message': 'The document name does not match the id!'}, 404)

    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to delete this object."}, 403)

    if len(timdb.documents.get_document_names(doc_id, include_nonpublic=True)) < 2:
        return jsonResponse({'message': "You can't delete the only name the document has."}, 403)

    parent_folder, _ = timdb.folders.split_location(alias)

    if not canWriteToFolder(parent_folder):
        return jsonResponse({'message': "You don't have permission to write to that folder."}, 403)

    timdb.documents.delete_name(doc_id, alias)
    return "Success"


@manage_page.route("/rename/<int:doc_id>", methods=["PUT"])
def rename_folder(doc_id):
    timdb = getTimDb()
    new_name = request.get_json()['new_name'].strip('/')

    if timdb.documents.exists(doc_id):
        return jsonResponse({'message': 'Rename route is no longer supported for documents.'}, 403)

    if not timdb.folders.exists(doc_id):
        return jsonResponse({'message': 'The folder does not exist!'}, 404)

    if timdb.documents.get_document_id(new_name) is not None or timdb.folders.get_folder_id(new_name) is not None:
        return jsonResponse({'message': 'Item with the same name already exists.'}, 403)

    if not timdb.users.has_manage_access(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to rename this object."}, 403)

    parent, _ = timdb.folders.split_location(new_name)

    if timdb.folders.get_folder_id(parent) is None:
        # Maybe do a recursive create with permission checks here later?
        return jsonResponse({'message': "The location does not exist."}, 403)

    if not canWriteToFolder(parent):
        return jsonResponse({'message': "You don't have permission to write to that folder."}, 403)

    timdb.folders.rename(doc_id, new_name)
    return "Success"


@manage_page.route("/getPermissions/<int:doc_id>")
def getPermissions(doc_id):
    timdb = getTimDb()
    isFolder = False
    if not timdb.documents.exists(doc_id):
        if timdb.folders.exists(doc_id):
            isFolder = True
        else:
            abort(404)
    if not timdb.users.has_manage_access(getCurrentUserId(), doc_id):
        abort(403)

    if isFolder:
        doc_data = timdb.folders.get(doc_id)
        doc_data['name'] = doc_data.pop('fullname')
        doc_data.pop('location')
    else:
        doc_data = timdb.documents.get_document(doc_id)

    grouprights = timdb.users.get_rights_holders(doc_id)
    return jsonResponse({'doc': doc_data, 'grouprights': grouprights})


@manage_page.route("/documents/<int:doc_id>", methods=["DELETE"])
def deleteDocument(doc_id):
    timdb = getTimDb()
    if not timdb.documents.exists(doc_id):
        return jsonResponse({'message': 'Document does not exist.'}, 404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to delete this document."}, 403)
    timdb.documents.delete(doc_id)
    return "Success"


@manage_page.route("/folders/<int:doc_id>", methods=["DELETE"])
def deleteFolder(doc_id):
    timdb = getTimDb()
    if not timdb.folders.exists(doc_id):
        return jsonResponse({'message': 'Folder does not exist.'}, 404)
    if not timdb.users.userIsOwner(getCurrentUserId(), doc_id):
        return jsonResponse({'message': "You don't have permission to delete this folder."}, 403)
    if not timdb.folders.is_empty(doc_id):
        return jsonResponse({'message': "The folder is not empty. Only empty folders can be deleted."}, 403)

    timdb.folders.delete(doc_id)
    return "Success"
