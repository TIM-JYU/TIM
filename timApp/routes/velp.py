from flask import Blueprint
from .common import *
import os.path

velps = Blueprint('velps',
                  __name__,
                  url_prefix='')


# TODO: Add document handling for all velp group related stuff
# TODO: Done create velp, get velp groups from folders (get_velp_groups),
# TODO: make default velp group and necessary folder (velpabc)

@velps.route("/<document_id>/get_default_velp_group", methods=['GET'])
def get_default_velp_group(document_id: int):
    """Get default velp group id and if default velp group doesn't exist yet, create one

    :return: Doc id
    """
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")

    timdb = getTimDb()
    user_id = getCurrentUserId()

    owner_group_id = timdb.documents.get_owner(doc_id)
    print(owner_group_id)
    full_path = timdb.documents.get_first_document_name(doc_id)
    doc_name = os.path.basename(full_path)

    # Check if document's path contains velp groups folder and if it does, make document its own default velp group
    if "velp groups/" in full_path:
        timdb.velp_groups.make_document_a_velp_group(doc_name, doc_id)
        velp_group = [{'target_type': '0', 'target_id': 0, 'id': doc_id}]
        timdb.velp_groups.add_groups_to_selection_table(velp_group, doc_id, user_id)
        print("Document is a velp group, made default velp group to point itself")
        return jsonResponse({"id": doc_id, "name": "Default"})

    # Problems arise if document is located in [root] folder, this check fixes that
    if len(full_path) - len(doc_name) - 1 < len(doc_name):
        doc_path = ""
    else:
        doc_path = full_path[:len(full_path) - len(doc_name) - 1]

    # Get velp group folder path and if necessary, creates those folders
    velps_folder_path = timdb.folders.check_velp_group_folder_path(doc_path, owner_group_id, doc_name)

    velp_groups = timdb.documents.get_documents_in_folder(velps_folder_path)
    default_velp_group = False
    default_group_name = doc_name + "_default"
    # Check through all documents in velp group folder to check if default group exists
    if velp_groups is not None:
        for group in velp_groups:
            if group['name'] == velps_folder_path + "/" + default_group_name:
                default_velp_group = True
                doc_id = group['id']
                break

    # If default didn't exists yet (or there were no documents / velp groups to start with), create one
    if default_velp_group is False:
        # Check that current user is owner for the document as well
        if timdb.users.user_is_owner(user_id, doc_id) is False:
            abort(400, "User is not owner of the document, can't create default velp group")
        default_group_path = velps_folder_path + "/" + default_group_name
        # new_group = timdb.documents.create(default_group_path, owner_group_id)
        # new_group_id = new_group.doc_id
        doc_id = timdb.velp_groups.create_default_velp_group(default_group_name, owner_group_id, default_group_path)
        print("Default group didn't exist, created one with id: " + str(doc_id))

    return jsonResponse({"id": doc_id, "name": "Default"})


@velps.route("/<document_id>/get_velps", methods=['GET'])
def get_velps(document_id: int):
    """Get all velps for document user has access to

    :param document_id: ID of document
    :return: List of velps as dictionaries containing all needed information
    """
    timdb = getTimDb()
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    velp_groups = get_velp_groups_from_tree(doc_id)
    user_id = getCurrentUserId()
    timdb.velp_groups.add_groups_to_selection_table(velp_groups, doc_id, user_id)

    user_groups = timdb.users.get_user_groups(user_id)
    user_group_list = []
    for group in user_groups:
        user_group_list.append(group['id'])
    imported_groups = timdb.velp_groups.get_groups_from_imported_table(user_group_list, doc_id)
    timdb.velp_groups.add_groups_to_selection_table(imported_groups, doc_id, user_id)
    velp_content = timdb.velps.get_velp_content_for_document(doc_id, user_id)

    return jsonResponse(velp_content)


@velps.route("/<document_id>/get_velp_groups", methods=['GET'])
def get_velp_groups(document_id: int):
    """Gets all velp groups for document user has access to by using VelpGroupSelection table

    :param document_id: ID of document
    :return:
    """
    timdb = getTimDb()
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    user_id = getCurrentUserId()
    velp_groups = timdb.velp_groups.get_groups_from_selection_table(doc_id, user_id)
    # SQLite uses 1/0 instead of True/False, change them to True/False for JavaScript side
    for group in velp_groups:
        if group['selected'] is 1:
            group['selected'] = True
        else:
            group['selected'] = False
        group['edit_access'] = timdb.users.has_edit_access(user_id, group['id'])

    return jsonResponse(velp_groups)

@velps.route("/<document_id>/get_velp_group_defaults", methods=['GET'])
def get_velp_group_defaults(document_id: int):
    """Gets default velp group selections for velp groups user has access to in document

    :param document_id: ID of document
    :return:
    """
    timdb = getTimDb()
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    user_id = getCurrentUserId()
    velp_group_defaults = timdb.velp_groups.get_groups_from_defaults_table(doc_id)
    for group in velp_group_defaults:
        if timdb.users.has_view_access(user_id, group['id']) is False:
            velp_group_defaults.remove(group)

    return jsonResponse(velp_group_defaults)


@velps.route("/<document_id>/get_velp_labels", methods=['GET'])
def get_velp_labels(document_id: int) -> 'str':
    """Gets all velp labels for document user has access to by using VelpGroupSelection table

    :param document_id: ID of document
    :return:
    """
    timdb = getTimDb()
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    # Todo select language.
    label_data = timdb.velps.get_velp_label_content_for_document(doc_id, getCurrentUserId())
    response = jsonResponse(label_data)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/add_velp", methods=['POST'])
def add_velp():
    """Creates a new velp and adds it to velp groups user chose

    :return: ID of new velp
    """
    json_data = request.get_json()
    try:
        velp_content = json_data['content']
        velp_groups = json_data['velp_groups']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    if not velp_content:
        abort(400, "Empty content string.")

    # Optional stuff
    # .get returns null instead of throwing if data is missing.
    default_points = json_data.get('points')
    language_id = json_data.get('language_id')
    icon_id = json_data.get('icon_id')
    valid_until = json_data.get('valid_until')
    velp_labels = json_data.get('labels')

    default_points = float(default_points) if default_points is not None else None
    icon_id = int(icon_id) if icon_id is not None else None

    timdb = getTimDb()
    verifyLoggedIn()
    current_user_id = getCurrentUserId()

    velp_groups_rights = []

    can_add_velp = False

    # Check where user has edit rights and only add new velp to those
    for group in velp_groups:
        if timdb.users.has_edit_access(current_user_id, group) is True:
            can_add_velp = True
            velp_groups_rights.append(group)
        else:
            print("No edit access for velp group:", group)

    if can_add_velp is False:
        abort(403, "Can't add velp without any velp groups")

    velp_groups = velp_groups_rights

    new_velp_id = timdb.velps.create_new_velp(current_user_id, velp_content, default_points,
                                              icon_id, valid_until, language_id)

    if velp_labels is not None:
        timdb.velps.add_labels_to_velp(new_velp_id, velp_labels)
    if velp_groups is not None:
        for group_id in velp_groups:
            print(new_velp_id)
            print(group_id)
            timdb.velp_groups.add_velp_to_group(new_velp_id, group_id)
    else:
        abort(400, "No velp groups")

    return jsonResponse(new_velp_id)


@velps.route("/<document_id>/update_velp", methods=['POST'])
def update_velp(document_id: int):
    """Updates velp data

    :return:
    """
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")

    try:
        json_data = request.get_json()
        velp_id = json_data.get('id')
        new_content = json_data.get('content')
        language_id = json_data.get('language_id')
        velp_groups = json_data['velp_groups']
    except KeyError as e:
        abort(400, "Missing data " + e.args[0])
    if not new_content:
        abort(400, "Empty content string.")

    default_points = json_data.get('points')
    icon_id = json_data.get('icon_id')
    new_labels = json_data.get('labels')
    timdb = getTimDb()
    verifyLoggedIn()
    user_id = getCurrentUserId()
    edit_access = False

    all_velp_groups = timdb.velp_groups.get_groups_for_velp(velp_id)

    # Check that user has edit access to velp via any velp group in database
    for group in all_velp_groups:
        if timdb.users.has_edit_access(user_id, group['id']) is True:
            edit_access = True
            break
    if edit_access is False:
        abort(403, "No edit access to velp via any velp group.")

    # Check which velp groups velp should belong to after update
    edit_access = False
    groups_to_remove = []
    groups_to_add = []

    # Add all velp group ids user edit has access to in a document to a list
    doc_groups = timdb.velp_groups.get_groups_from_selection_table(doc_id, user_id)
    for group in doc_groups:
        if timdb.users.has_edit_access(user_id, group['id']) is True:
            groups_to_remove.append(group['id'])
            edit_access = True
        else:
            print("No edit access to group", group[id])

    # Check that user has edit access to velp groups in given velp group list and add them to a new list
    for group in velp_groups:
        if timdb.users.has_edit_access(user_id, group) is True:
            edit_access = True
            groups_to_add.append(group)

        else:
            print("No edit access to group", group)

    # Add and remove velp from velp groups
    if edit_access is True:
        timdb.velp_groups.remove_velp_from_groups(velp_id, groups_to_remove)
        timdb.velp_groups.add_velp_to_groups(velp_id, groups_to_add)

    old_content = timdb.velps.get_latest_velp_version(velp_id, language_id)
    old_labels = timdb.velps.get_velp_label_ids_for_velp(velp_id)
    if old_content != new_content:
        # Todo this does not really work correctly, now any update to any language creates a new version, and we can not
        # produce different contents with the same version but different language.

        version_id = timdb.velps.create_velp_version(velp_id)
        timdb.velps.create_velp_content(version_id, language_id, new_content)
    if old_labels != new_labels:
        timdb.velps.update_velp_labels(velp_id, new_labels)
    timdb.velps.update_velp(velp_id, default_points, icon_id)
    return ""  # TODO: return something more informative


@velps.route("/add_velp_label", methods=["POST"])
def add_label():
    """Creates new velp label

    :return: ID of new velp label
    """
    json_data = request.get_json()
    try:
        content = json_data['content']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    language_id = json_data.get('language_id')
    language_id = "FI" if language_id is None else language_id

    timdb = getTimDb()
    label_id = timdb.velps.create_velp_label(language_id, content)

    return jsonResponse(label_id)


@velps.route("/update_velp_label", methods=["POST"])
def update_velp_label():
    """Updates velp label content

    :return:
    """
    json_data = request.get_json()
    print(json_data)
    try:
        content = json_data['content']
        print(content)
        velp_label_id = json_data['id']
        print(velp_label_id)
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    language_id = json_data.get('language_id')
    language_id = "FI" if language_id is None else language_id

    timdb = getTimDb()
    # TODO: Add some check so a random person can't use the route?
    timdb.velps.update_velp_label(velp_label_id, language_id, content)

    return ""


@velps.route("/<document_id>/change_selection", methods=["POST"])
def change_selection(document_id: int):
    """Change selection for velp group in users VelpGroupSelection in current document

    :param document_id: ID of document
    :return:
    """
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")

    json_data = request.get_json()
    try:
        velp_group_id = json_data['id']
        selection = json_data['selected']
        target_type = json_data['target_type']
        target_id = json_data['target_id']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    verifyLoggedIn()
    user_id = getCurrentUserId()
    timdb = getTimDb()
    timdb.velp_groups.change_selection(doc_id, velp_group_id, target_type, target_id, user_id, selection)

    return ""

@velps.route("/<document_id>/change_default_selection", methods=["POST"])
def change_default_selection(document_id: int):
    """Change selection for velp group in users VelpGroupSelection in current document

    :param document_id: ID of document
    :return:
    """
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")

    json_data = request.get_json()
    try:
        velp_group_id = json_data['id']
        target_type = json_data['target_type']
        target_id = json_data['target_id']
        selection = json_data['selected']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    verifyLoggedIn()
    user_id = getCurrentUserId()
    timdb = getTimDb()
    if timdb.users.has_manage_access_access(user_id, doc_id) is True:
        timdb.velp_groups.change_default_selection(doc_id, target_type, target_id, velp_group_id, selection)

    return ""

@velps.route("/<document_id>/create_velp_group", methods=['POST'])
def create_velp_group(document_id: int):
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")

    json_data = request.get_json()
    try:
        velp_group_name = json_data.get('name')
        target_type = json_data.get('target_type')
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])

    timdb = getTimDb()

    full_path = timdb.documents.get_first_document_name(doc_id)
    doc_name = os.path.basename(full_path)
    doc_path = full_path[:len(full_path) - len(doc_name) - 1]
    if len(doc_path) < len(doc_name):   # If document is located in root folder
        doc_path = ""

    # valid_until = json_data.get('valid_until')

    verifyLoggedIn()
    user_group_id = getCurrentUserGroup()

    # Create a new velp group / document in users/username/velp groups folder
    if target_type == 0:
        user = getCurrentUserName()
        user_velp_path = timdb.folders.check_personal_velp_folder(user, user_group_id)
        new_group_path = user_velp_path + "/" + velp_group_name
        group_exists = timdb.documents.resolve_doc_id_name(new_group_path)
        if group_exists is None:
            # new_group = timdb.documents.create(new_group_path, user_group_id)
            # new_group_id = new_group.doc_id
            velp_group_id = timdb.velp_groups.create_velp_group(velp_group_name, user_group_id, new_group_path)
        else:
            abort(400, "Velp group with same name and location exists already.")

        return jsonResponse(velp_group_id)

    # TODO: Who has can add velp groups to documents or folders
    # TODO: Also who owns the new velp group? Now it is current user
    if target_type == 2:
        doc_name = ""
    # Gives path to either velp groups or velp groups/document name folder
    velps_folder_path = timdb.folders.check_velp_group_folder_path(doc_path, user_group_id, doc_name)

    new_group_path = velps_folder_path + "/" + velp_group_name
    group_exists = timdb.documents.resolve_doc_id_name(new_group_path)  # Check name so no duplicates are made
    if group_exists is None:
        velp_group_id = timdb.velp_groups.create_velp_group(velp_group_name, user_group_id, new_group_path)
    else:
        abort(400, "Velp group with same name and location exists already.")


    return jsonResponse(velp_group_id)


def get_velp_groups_from_tree(document_id: int):
    """Returns all velp groups found from tree from document to root and from users own velp folder

    Checks document's own velp group folder first, then default velp group folders going up all the
    way to root. Doesn't branch side ways or down, only checks parents. After root has been reached,
    finally checks users own velp group folder.

    Checks that user has minimum of view right for velp groups.
    :return: List of document / velp group information of found hits.
    """

    doc_id = int(document_id)
    timdb = getTimDb()
    full_path = timdb.documents.get_first_document_name(doc_id)
    doc_name = os.path.basename(full_path)
    doc_path = full_path[:len(full_path) - len(doc_name) - 1]
    velp_group_folder = "velp groups"

    current_path = doc_path
    velp_groups_path = current_path + "/" + velp_group_folder
    doc_velp_path = velp_groups_path + "/" + doc_name
    username = getCurrentUserName()
    personal_velps_path = "users/" + username + "/" + velp_group_folder
    owner_group_id = 3  # TODO: Choose owner group correctly, now uses All Korppi users

    velp_groups = []

    # Velp groups for areas, plugins etc
    deeper_path = timdb.folders.get_folders(doc_velp_path)
    for path in deeper_path:
        full_path = path['fullname']
        found_velp_groups = timdb.documents.get_documents_in_folder(full_path)
        for v in found_velp_groups:
            if timdb.users.has_view_access(getCurrentUserId(), timdb.documents.get_document_id(v['name'])):
                v['target_type'] = 1
                v['target_id'] = path['name']
                velp_groups.append(v)

    # Document's own velp group
    found_velp_groups = timdb.documents.get_documents_in_folder(current_path + "/" + velp_group_folder + "/" + doc_name)
    for v in found_velp_groups:
        if timdb.users.has_view_access(getCurrentUserId(), timdb.documents.get_document_id(v['name'])):
            v['target_type'] = 0
            v['target_id'] = 0
            velp_groups.append(v)

    # Velp group folder when going towards root in tree
    while True:
        if current_path != '':
            found_velp_groups = timdb.documents.get_documents_in_folder(current_path + "/" + velp_group_folder)
        else:
            found_velp_groups = timdb.documents.get_documents_in_folder(velp_group_folder)
        for v in found_velp_groups:
            if timdb.users.has_view_access(getCurrentUserId(), timdb.documents.get_document_id(v['name'])):
                v['target_type'] = 0
                v['target_id'] = 0
                velp_groups.append(v)
        if current_path == '':
            break
        current_path, _ = timdb.folders.split_location(current_path)

    # User's own velp groups
    found_velp_groups = timdb.documents.get_documents_in_folder(personal_velps_path)
    for v in found_velp_groups:
        if timdb.users.has_view_access(getCurrentUserId(), timdb.documents.get_document_id(v['name'])):
            v['target_type'] = 0
            v['target_id'] = 0
            velp_groups.append(v)

    results = [dict(t) for t in set(tuple(d.items()) for d in velp_groups)]  # Remove possible doubles

    # Add found documents to VelpGroup table if they weren't there yet
    for result in results:
        id_number = result['id']
        is_velp_group = timdb.velp_groups.is_id_velp_group(id_number)
        if is_velp_group is False:
            group_name = os.path.basename(timdb.documents.get_first_document_name(id_number))
            timdb.velp_groups.make_document_a_velp_group(group_name, id_number)

    return results
