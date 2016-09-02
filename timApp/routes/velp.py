"""
The module handles the main logic related to velps, velp groups and labels. This includes adding and modifiying velps
and labels as well as adding new velp groups. The module also retrieves or creates the default velp group for the
document and the personal default group for the user. Velp groups can be set to shown or shown as default in specific
element (or in the whole document) through this module. The module also retrieves the velps, velp groups and labels to
the document.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

from typing import Dict
from flask import Blueprint
from .common import *

velps = Blueprint('velps',
                  __name__,
                  url_prefix='')


# TODO: Add document handling for all velp group related stuff
# TODO: Done create velp, get velp groups from folders (get_velp_groups),
# TODO: make default velp group and necessary folder (velpabc)


@velps.route("/<int:doc_id>/get_default_velp_group", methods=['GET'])
def get_default_velp_group(doc_id: int) -> Dict:
    """Get default velp group ID and if  velp group doesn't exist yet, create one.

    :param doc_id: ID of document
    :return: Dictionary containing default velp group's ID and name
    """
    timdb = getTimDb()
    user_id = getCurrentUserId()

    owner_group_id = timdb.documents.get_owner(doc_id)
    full_path = timdb.documents.get_first_document_name(doc_id)
    doc_path, doc_name = timdb.documents.split_location(full_path)
    edit_access = False
    if timdb.users.has_edit_access(user_id, doc_id):
        edit_access = True
    else:
        return set_no_cache_headers(
            jsonResponse({"id": -1, "name": "No access to default group", "edit_access": edit_access}))

    # Check if document's path contains velp groups folder and if it does, make document its own default velp group.
    # This default document declaration isn't saved to database (else eventually all velp groups might be defaults).
    if "velp groups/" in full_path:
        if timdb.users.has_edit_access(user_id, doc_id):
            edit_access = True
        else:
            return set_no_cache_headers(
                jsonResponse({"id": -1, "name": "No access to default group", "edit_access": edit_access}))
        timdb.velp_groups.make_document_a_velp_group(doc_name, doc_id)
        velp_group = [{'target_type': '0', 'target_id': 0, 'id': doc_id}]

        timdb.velp_groups.add_groups_to_document(velp_group, doc_id, user_id)
        timdb.velp_groups.add_groups_to_selection_table(velp_group, doc_id, user_id)
        print("Document is a velp group, made default velp group to point itself")
        return set_no_cache_headers(jsonResponse({"id": doc_id, "name": doc_name, "edit_access": edit_access}))

    if doc_path != "":
        found_velp_groups = timdb.documents.get_documents_in_folder(doc_path + "/" + "velp groups" + "/" + doc_name)
    else:  # Documents in root folder don't like / after empty path
        found_velp_groups = timdb.documents.get_documents_in_folder("velp groups" + "/" + doc_name)
    velp_groups = []
    for v in found_velp_groups:
        # if timdb.users.has_view_access(user_id, timdb.documents.get_document_id(v['name'])):
        velp_groups.append(v['id'])
    default_group = timdb.velp_groups.check_velp_group_ids_for_default_group(velp_groups)
    if default_group is not None:
        default_group["edit_access"] = timdb.users.has_edit_access(user_id,default_group['id'])
        return set_no_cache_headers(jsonResponse(default_group))

    response = jsonResponse({"id": -1, "name": doc_name + "_default", "edit_access": edit_access})
    return set_no_cache_headers(response)


@velps.route("/get_default_personal_velp_group", methods=['GET'])
def get_default_personal_velp_group() -> Dict:
    """ Get default personal velp group ID and if velp group doesn't exist yet, create one.

    :return: Dictionary containing personal velp group data.
    """
    timdb = getTimDb()
    user_name = getCurrentUserName()

    personal_velp_group_path = "users/" + user_name + "/velp groups"
    found_velp_groups = timdb.documents.get_documents_in_folder(personal_velp_group_path)
    velp_groups = []
    for v in found_velp_groups:
        velp_groups.append(v['id'])
    default_group = timdb.velp_groups.check_velp_group_ids_for_default_group(velp_groups)
    if default_group is not None:
        return set_no_cache_headers(jsonResponse(default_group))
    else:
        group_name = "Personal default"
        new_group_path = personal_velp_group_path + "/" + group_name
        group_exists = timdb.documents.resolve_doc_id_name(new_group_path)
        if group_exists:
            default_id = timdb.documents.get_document_id(new_group_path)
            timdb.velp_groups.update_velp_group_to_default_velp_group(default_id)
            created_new = False
        else:
            user_group = getCurrentUserGroup()
            default_id = timdb.velp_groups.create_default_velp_group(group_name, user_group, new_group_path)
            created_new = True

        created_velp_group = dict()
        created_velp_group['id'] = default_id
        created_velp_group['target_type'] = 0
        created_velp_group['target_id'] = "0"
        created_velp_group['name'] = group_name
        created_velp_group['location'] = new_group_path
        created_velp_group['show'] = True
        created_velp_group['default'] = False
        created_velp_group['edit_access'] = True
        created_velp_group['default_group'] = True
        created_velp_group['created_new_group'] = created_new

        response = set_no_cache_headers(jsonResponse(created_velp_group))
        return response


@velps.route("/<int:doc_id>/get_velps", methods=['GET'])
def get_velps(doc_id: int):
    """Get all velps for document user has access to.

    :param doc_id: ID of document
    :return: List of velps as dictionaries containing all needed information
    """
    timdb = getTimDb()

    user_id = getCurrentUserId()
    velp_content = timdb.velps.get_velp_content_for_document(doc_id, user_id)

    response = jsonResponse(velp_content)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/get_velp_groups", methods=['GET'])
def get_velp_groups(doc_id: int):
    """Gets all velp groups for document user has access to by using VelpGroupSelection table.

    :param doc_id: ID of document
    :return: List of dictionaries containing velp group information
    """
    timdb = getTimDb()
    user_id = getCurrentUserId()

    velp_groups = get_velp_groups_from_tree(doc_id)
    timdb.velp_groups.add_groups_to_document(velp_groups, doc_id, user_id)

    user_groups = timdb.users.get_user_groups(user_id)
    user_group_list = []
    for group in user_groups:
        user_group_list.append(group['id'])
    imported_groups = timdb.velp_groups.get_groups_from_imported_table(user_group_list, doc_id)
    timdb.velp_groups.add_groups_to_document(imported_groups, doc_id, user_id)

    all_velp_groups = timdb.velp_groups.get_groups_from_document_table(doc_id, user_id)

    # if timdb.users.has_manage_access(user_id, doc_id):
    #    timdb.velp_groups.add_groups_to_default_table(all_velp_groups, doc_id)

    # SQLite uses 1/0 instead of True/False, change them to True/False for JavaScript side
    for group in all_velp_groups:
        group['edit_access'] = timdb.users.has_edit_access(user_id, group['id'])

    response = jsonResponse(all_velp_groups)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/get_velp_group_personal_selections", methods=['GET'])
def get_velp_group_personal_selections(doc_id: int) -> Dict:
    """Gets default velp group selections for velp groups user has access to in document.

    :param doc_id: ID of document
    :return: Dictionary containing list of selected velp groups for each target area IDs
    """
    timdb = getTimDb()
    user_id = getCurrentUserId()
    velp_group_selections = timdb.velp_groups.get_personal_selections_for_velp_groups(doc_id, user_id)

    response = jsonResponse(velp_group_selections)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/get_velp_group_default_selections", methods=['GET'])
def get_velp_group_default_selections(doc_id: int) -> Dict:
    """Gets default velp group selections for velp groups user has access to in document.

    :param doc_id: ID of document
    :return: Dictionary containing list of default velp groups for each target area IDs
    """
    timdb = getTimDb()
    user_id = getCurrentUserId()
    velp_group_defaults = timdb.velp_groups.get_default_selections_for_velp_groups(doc_id, user_id)

    response = jsonResponse(velp_group_defaults)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/get_velp_labels", methods=['GET'])
def get_velp_labels(doc_id: int) -> 'str':
    """Gets all velp labels for document user has access to by using VelpGroupSelection table.

    :param doc_id: ID of document
    :return: List of dicts containing velp label IDs and content for the document
    """
    timdb = getTimDb()
    # Todo select language.
    label_data = timdb.velps.get_velp_label_content_for_document(doc_id, getCurrentUserId())

    response = jsonResponse(label_data)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/add_velp", methods=['POST'])
def add_velp() -> int:
    """Creates a new velp and adds it to velp groups user chose.

    Required key(s):
        - content: content of the new velp
        - velp_groups: list of velp group IDs of the new velp.

    Optional key(s):
        - points: velp points
        - language_id: language ID
        - icon_id: icon ID
        - valid_until: time stamp to until velp is still valid
        - labels: labels of the velp
        - visible_to: visibility group of the velp (1-4)

    :return: ID of new velp
    """
    json_data = request.get_json()
    try:
        velp_content = json_data['content']
        velp_groups = json_data['velp_groups']
    except KeyError as e:
        return abort(400, "Missing data: " + e.args[0])
    if not velp_content:
        return abort(400, "Empty content string.")

    # Optional stuff
    # .get returns null instead of throwing if data is missing.
    default_points = json_data.get('points')
    language_id = json_data.get('language_id')
    icon_id = json_data.get('icon_id')
    valid_until = json_data.get('valid_until')
    velp_labels = json_data.get('labels')
    visible_to = json_data.get('visible_to')

    default_points = float(default_points) if default_points is not None else None
    icon_id = int(icon_id) if icon_id is not None else None

    timdb = getTimDb()
    verifyLoggedIn()
    current_user_id = getCurrentUserId()

    velp_groups_rights = []

    can_add_velp = False

    # Check where user has edit rights and only add new velp to those
    for group in velp_groups:
        if timdb.users.has_edit_access(current_user_id, group):
            can_add_velp = True
            velp_groups_rights.append(group)
        else:
            print("No edit access for velp group:", group)

    if not can_add_velp:
        return abort(400, "Can't add velp without any velp groups")

    velp_groups = velp_groups_rights

    new_velp_id = timdb.velps.create_new_velp(current_user_id, velp_content, default_points,
                                              icon_id, valid_until, language_id, visible_to)

    if velp_labels is not None:
        timdb.velps.add_labels_to_velp(new_velp_id, velp_labels)
    if velp_groups is not None:
        for group_id in velp_groups:
            timdb.velp_groups.add_velp_to_group(new_velp_id, group_id)
    else:
        return abort(400, "No velp groups")

    response = jsonResponse(new_velp_id)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/update_velp", methods=['POST'])
def update_velp(doc_id: int):
    """Updates the velp's data.

    Required key(s):
        - id: velp ID
        - content: velp content
        - language_id: language ID
        - velp groups: list of velp group IDs.

    Optional key(s):
        - points: velp points
        - icon_id: velp icon
        - labels: velp labels

    :param doc_id: ID of document
    :return: okJsonResponse
    """

    try:
        json_data = request.get_json()
        velp_id = json_data.get('id')
        new_content = json_data.get('content')
        language_id = json_data.get('language_id')
        velp_groups = json_data['velp_groups']
    except KeyError as e:
        return abort(400, "Missing data " + e.args[0])
    if not new_content:
        return abort(400, "Empty content string.")

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
        if timdb.users.has_edit_access(user_id, group['id']):
            edit_access = True
            break
    if not edit_access:
        return abort(403, "No edit access to velp via any velp group.")

    # Check which velp groups velp should belong to after update
    edit_access = False
    groups_to_remove = []
    groups_to_add = []

    # Add all velp group ids user has edit access to in a document to a remove list
    doc_groups = timdb.velp_groups.get_groups_from_document_table(doc_id, user_id)
    for group in doc_groups:
        if timdb.users.has_edit_access(user_id, group['id']):
            groups_to_remove.append(group['id'])
            edit_access = True

    # Check that user has edit access to velp groups in given velp group list and add them to an add list
    for group in velp_groups:
        if timdb.users.has_edit_access(user_id, group):
            edit_access = True
            groups_to_add.append(group)

    # Add and remove velp from velp groups
    if edit_access and len(groups_to_add) > 0:
        timdb.velp_groups.remove_velp_from_groups(velp_id, groups_to_remove)
        timdb.velp_groups.add_velp_to_groups(velp_id, groups_to_add)

    old_content = timdb.velps.get_latest_velp_version(velp_id, language_id)['content']
    old_labels = timdb.velps.get_velp_label_ids_for_velp(velp_id)
    if old_content != new_content:
        # Todo this does not really work correctly, now any update to any language creates a new version, and we can not
        # produce different contents with the same version but different language.

        version_id = timdb.velps.create_velp_version(velp_id)
        timdb.velps.create_velp_content(version_id, language_id, new_content)
    if old_labels != new_labels:
        timdb.velps.update_velp_labels(velp_id, new_labels)
    timdb.velps.update_velp(velp_id, default_points, icon_id)

    response = okJsonResponse()
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/add_velp_label", methods=["POST"])
def add_label() -> int:
    """Creates new velp label.

    Required key(s):
        - content: label content

    Optional key(s):
        - language_id: language ID of the label.

    :return: ID of new velp label
    """
    json_data = request.get_json()
    try:
        content = json_data['content']
    except KeyError as e:
        return abort(400, "Missing data: " + e.args[0])
    language_id = json_data.get('language_id')
    language_id = "FI" if language_id is None else language_id

    timdb = getTimDb()
    label_id = timdb.velps.create_velp_label(language_id, content)

    return str(label_id)


@velps.route("/update_velp_label", methods=["POST"])
def update_velp_label():
    """Updates velp label content.

    Required key(s):
        - content: label content
        - id: label ID.

    :return: okJsonResponse
    """
    json_data = request.get_json()
    try:
        content = json_data['content']
        velp_label_id = json_data['id']
    except KeyError as e:
        return abort(400, "Missing data: " + e.args[0])
    language_id = json_data.get('language_id')
    language_id = "FI" if language_id is None else language_id

    timdb = getTimDb()
    # TODO: Add some check so a random person can't use the route?
    timdb.velps.update_velp_label(velp_label_id, language_id, content)

    response = okJsonResponse()
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/change_selection", methods=["POST"])
def change_selection(doc_id: int):
    """Change selection for velp group in users VelpGroupSelection in current document.

    Required key(s):
        - id: velp group iD
        - target_type: target type of the selection (document, paragraph)
        - target_id: target id of the selection (paragraph id or 0 for the whole document)
        - selection_type: 'show' or 'default'.

    :param doc_id: ID of document
    :return: okJsonResponse
    """

    json_data = request.get_json()
    try:
        velp_group_id = json_data['id']
        target_type = json_data['target_type']
        target_id = json_data['target_id']
        selection_type = json_data['selection_type']
    except KeyError as e:
        return abort(400, "Missing data: " + e.args[0])
    verifyLoggedIn()
    user_id = getCurrentUserId()
    timdb = getTimDb()
    if selection_type == "show":
        try:
            selection = json_data['show']
        except KeyError as e:
            return abort(400, "Missing data: " + e.args[0])
        timdb.velp_groups.change_selection(doc_id, velp_group_id, target_type, target_id, user_id, selection)
    elif selection_type == "default" and timdb.users.has_manage_access(user_id, doc_id):
        try:
            selection = json_data['default']
        except KeyError as e:
            return abort(400, "Missing data: " + e.args[0])
        timdb.velp_groups.change_default_selection(doc_id, velp_group_id, target_type, target_id, selection)

    response = okJsonResponse()
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/change_all_selections", methods=["POST"])
def change_all_selections(doc_id: int):
    """Change selection for velp group in users VelpGroupSelection in current document.

    Required key(s):
        - selection: 1 or 0 (true or false)
        - target_type: target type of the selection (document, paragraph)
        - target_id: target id of the selection (paragraph id or 0 for the whole document)
        - selection_type: 'show' or 'default'.

    :param doc_id: ID of document
    :return: okJsonResponse
    """

    json_data = request.get_json()
    try:
        selection = json_data['selection']
        target_type = json_data['target_type']
        target_id = json_data['target_id']
        selection_type = json_data['selection_type']
    except KeyError as e:
        return abort(400, "Missing data: " + e.args[0])
    verifyLoggedIn()
    user_id = getCurrentUserId()
    timdb = getTimDb()
    if selection_type == "show":
        timdb.velp_groups.change_all_target_area_selections(doc_id, target_type, target_id, user_id, selection)
    elif selection_type == "default" and timdb.users.has_manage_access(user_id, doc_id):
        timdb.velp_groups.change_all_target_area_default_selections(doc_id, target_type, target_id, user_id, selection)

    response = okJsonResponse()
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/change_default_selection", methods=["POST"])
def change_default_selection(doc_id: int):
    """Change selection for velp group in users VelpGroupSelection in current document.

    Required key(s):
        - id: velp group ID
        - target_type: target type of the selection (document, paragraph)
        - target_id: target id of the selection (paragraph id or 0 for the whole document)
        - default: current default value.

    :param doc_id: ID of document
    :return: okJsonResponse
    """

    json_data = request.get_json()
    try:
        velp_group_id = json_data['id']
        target_type = json_data['target_type']
        target_id = json_data['target_id']
        selection = json_data['default']
    except KeyError as e:
        return abort(400, "Missing data: " + e.args[0])
    verifyLoggedIn()
    user_id = getCurrentUserId()
    timdb = getTimDb()
    if timdb.users.has_manage_access(user_id, doc_id):
        timdb.velp_groups.change_default_selection(doc_id, velp_group_id, target_type, target_id, selection)

    response = okJsonResponse()
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/reset_target_area_selections_to_defaults", methods=['POST'])
def reset_target_area_selections_to_defaults(doc_id: int):
    """Changes user's personal velp group selections in target area to defaults.

    Required key(s):
        - target_id: target id of the selection (paragraph id or 0 for the whole document)

    :param doc_id: ID of document
    :return: okJsonResponse()
    """

    json_data = request.get_json()
    try:
        target_id = json_data['target_id']
    except KeyError as e:
        return abort(400, "Missing data: " + e.args[0])

    timdb = getTimDb()
    user_id = getCurrentUserId()

    timdb.velp_groups.reset_target_area_selections_to_defaults(doc_id, target_id, user_id)

    response = okJsonResponse()
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/reset_all_selections_to_defaults", methods=['POST'])
def reset_all_selections_to_defaults(doc_id: int):
    """Changes user's all personal velp group selections in document to defaults.

    :param doc_id: ID of document
    :return: okJsonResponse()
    """

    timdb = getTimDb()
    user_id = getCurrentUserId()

    timdb.velp_groups.reset_all_selections_to_defaults(doc_id, user_id)

    response = okJsonResponse()
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/create_velp_group", methods=['POST'])
def create_velp_group(doc_id: int) -> Dict:
    """Creates a new velp group.

    Required key(s):
        - name: velp group name
        - target_type: document, folder or personal group.

    :param doc_id: ID of the document
    :return: Dictionary containing information of new velp group.
    """

    json_data = request.get_json()
    try:
        velp_group_name = json_data.get('name')
        target_type = json_data.get('target_type')
    except KeyError as e:
        return abort(400, "Missing data: " + e.args[0])

    timdb = getTimDb()

    full_path = timdb.documents.get_first_document_name(doc_id)
    doc_path, doc_name = timdb.documents.split_location(full_path)

    # valid_until = json_data.get('valid_until')

    verifyLoggedIn()
    user_group_id = getCurrentUserGroup()
    user_id = getCurrentUserId()

    # Create a new velp group / document in users/username/velp groups folder
    if target_type == 0:
        user_name = getCurrentUserName()
        user_velp_path = timdb.folders.check_personal_velp_folder(user_name, user_group_id)
        new_group_path = user_velp_path + "/" + velp_group_name
        group_exists = timdb.documents.resolve_doc_id_name(new_group_path)
        if group_exists is None:
            velp_group_id = timdb.velp_groups.create_velp_group(velp_group_name, user_group_id, new_group_path)
        else:
            return abort(400, "Velp group with same name and location exists already.")


    else:
        if target_type == 2:
            target_id = timdb.folders.get_folder_id(doc_path)
            doc_name = ""
        elif target_type == 1:
            target_id = doc_id
        else:
            return abort(400, "Unknown velp group target type.")

        if not timdb.users.has_edit_access(user_id, target_id):
            return abort(403, "Edit access is required.")

        # Gives path to either velp groups or velp groups/document name folder
        velps_folder_path = timdb.folders.check_velp_group_folder_path(doc_path, user_group_id, doc_name)

        new_group_path = velps_folder_path + "/" + velp_group_name
        group_exists = timdb.documents.resolve_doc_id_name(new_group_path)  # Check name so no duplicates are made
        if group_exists is None:
            original_owner = timdb.folders.get_owner(target_id)
            velp_group_id = timdb.velp_groups.create_velp_group(velp_group_name, original_owner, new_group_path)
            rights = timdb.users.get_rights_holders(target_id)
            # Copy all rights but view
            for right in rights:
                # TODO once someone implements a grant_access that takes access ids instead of strings, change to that
                # function.
                if not right['access_name'] == 'view':
                    timdb.users.grant_access(right['gid'], velp_group_id, right['access_name'])
        else:
            return abort(400, "Velp group with same name and location exists already.")

    created_velp_group = dict()
    created_velp_group['id'] = velp_group_id
    created_velp_group['target_type'] = 0
    created_velp_group['target_id'] = "0"
    created_velp_group['name'] = velp_group_name
    created_velp_group['location'] = new_group_path
    created_velp_group['selected'] = True
    created_velp_group['show'] = True
    created_velp_group['edit_access'] = True
    created_velp_group['default_group'] = False


    timdb.velp_groups.add_groups_to_document([created_velp_group], doc_id, user_id)
    # TODO Do we want to make just created velp group selected in document immediately?
    timdb.velp_groups.add_groups_to_selection_table([created_velp_group], doc_id, user_id)

    response = jsonResponse(created_velp_group)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


@velps.route("/<int:doc_id>/create_default_velp_group", methods=['POST'])
def create_default_velp_group(doc_id: int):
    """Creates a default velp group document or changes existing document to default velp group.

    :param doc_id: ID of document
    :return: Dictionary containing information of new default velp group.
    """

    timdb = getTimDb()

    full_path = timdb.documents.get_first_document_name(doc_id)
    doc_path, doc_name = timdb.documents.split_location(full_path)

    verifyLoggedIn()
    user_group_id = timdb.documents.get_owner(doc_id)
    user_id = getCurrentUserId()

    # if not timdb.users.is_user_id_in_group_id(user_id, user_group_id):
    #     print("User is not owner of current document")
    #     return abort(403, "User is not owner of current document")

    if not timdb.users.has_edit_access(user_id, doc_id):
        return abort(403, "User has no edit access to current document")

    velps_folder_path = timdb.folders.check_velp_group_folder_path(doc_path, user_group_id, doc_name)
    velp_group_name = doc_name + "_default"

    new_group_path = velps_folder_path + "/" + velp_group_name
    group_exists = timdb.documents.resolve_doc_id_name(new_group_path)  # Check name so no duplicates are made
    if group_exists is None:
        velp_group_id = timdb.velp_groups.create_default_velp_group(velp_group_name, user_group_id, new_group_path)
        created_new_group = True
        rights = timdb.users.get_rights_holders(doc_id)
        # Copy all rights but view
        for right in rights:
            # TODO once someone implements a grant_access that takes access ids instead of strings, change to that
            # function.
            if not right['access_name'] == 'view':
                timdb.users.grant_access(right['gid'], velp_group_id, right['access_name'])

    else:
        default_id = timdb.documents.get_document_id(new_group_path)
        velp_group_id = timdb.velp_groups.make_document_a_velp_group(velp_group_name, default_id, None, True)
        timdb.velp_groups.update_velp_group_to_default_velp_group(default_id)
        created_new_group = False

    created_velp_group = dict()
    created_velp_group['id'] = velp_group_id
    created_velp_group['target_type'] = 0
    created_velp_group['target_id'] = "0"
    created_velp_group['name'] = velp_group_name
    created_velp_group['location'] = new_group_path
    created_velp_group['selected'] = True
    created_velp_group['show'] = True
    created_velp_group['default'] = False
    created_velp_group['edit_access'] = True
    created_velp_group['default_group'] = True
    created_velp_group['created_new_group'] = created_new_group

    timdb.velp_groups.add_groups_to_document([created_velp_group], doc_id, user_id)

    # TODO Do we want to make just created default velp group selected in document immediately?
    # timdb.velp_groups.add_groups_to_selection_table([created_velp_group], doc_id, user_id)

    return jsonResponse(created_velp_group)


def get_velp_groups_from_tree(document_id: int):
    """Returns all velp groups found from tree from document to root and from users own velp folder

    Checks document's own velp group folder first, then default velp group folders going up all the
    way to root. Doesn't branch side ways or down, only checks parents. After root has been reached,
    finally checks users own velp group folder.

    Checks that user has minimum of view right for velp groups.

    :param document_id: ID of document
    :return: List of document / velp group information of found hits.
    """

    doc_id = int(document_id)
    timdb = getTimDb()
    full_path = timdb.documents.get_first_document_name(doc_id)
    doc_path, doc_name = timdb.documents.split_location(full_path)
    velp_group_folder = "velp groups"

    current_path = doc_path
    if current_path != "":
        velp_groups_path = current_path + "/" + velp_group_folder
    else:  # Documents in root folder don't like / after empty path
        velp_groups_path = velp_group_folder
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
                v['target_type'] = 0
                v['target_id'] = 0
                velp_groups.append(v)

    # Document's own velp group
    if current_path != "":
        found_velp_groups = timdb.documents.get_documents_in_folder(current_path + "/" + velp_group_folder +
                                                                    "/" + doc_name)
    else:  # Documents in root folder don't like / after empty path
        found_velp_groups = timdb.documents.get_documents_in_folder(velp_group_folder + "/" + doc_name)
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
        if not is_velp_group:
            _, group_name = timdb.documents.split_location(timdb.documents.get_first_document_name(id_number))
            timdb.velp_groups.make_document_a_velp_group(group_name, id_number)

    return results
