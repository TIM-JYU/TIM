from flask import Blueprint
from .common import *

velps = Blueprint('velps',
                  __name__,
                  url_prefix='')

# TODO: Add document handling for all velp group related stuff
# TODO: Done create velp, get velp groups from folders (get_velp_groups),
# TODO: make default velp group and necessary folder (velpabc)

@velps.route("/<document_id>/defaultvelpgroup", methods=['GET'])
def check_default_velp_group(document_id: int) -> str:
    timdb = getTimDb()

    # Make sure document_id is int
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")

    default_velp_group_id = timdb.velp_groups.check_default_group_exists(doc_id)
    if default_velp_group_id is None:
        default_group_name = timdb.documents.get_first_document_name(doc_id)
        default_velp_group_id = timdb.velp_groups.create_default_velp_group(default_group_name, 1)
        timdb.velp_groups.insert_group_to_document(int(default_velp_group_id), doc_id)
        print("Created a new default velp group, ID: " + str(default_velp_group_id) \
              + ", name: " + default_group_name + ", document ID: " + document_id)
        default_velp_group_id = [{'id': default_velp_group_id}]  # int to dictionary
    return jsonResponse(default_velp_group_id)


@velps.route("/<document_id>/<paragraph_id>/velps", methods=['GET'])
def get_velps(document_id: int, paragraph_id: str) -> str:
    timdb = getTimDb()
    # Make sure document_id is int
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")

    # TODO Remove following block, use route above instead
    default_exists = timdb.velp_groups.check_default_group_exists(doc_id)
    if default_exists is None:  # Create new default velp group if one does not exist yet
        default_group_name = timdb.documents.get_first_document_name(doc_id)
        new_default_id = timdb.velp_groups.create_default_velp_group(default_group_name, 1)
        timdb.velp_groups.insert_group_to_document(int(new_default_id), doc_id)
        print("Created a new default velp group, ID: " + str(new_default_id) \
              + ", name: " + default_group_name + ", document ID: " + document_id)
    # Todo Somehow communicate the language string for the get_document_velps function.

    velp_data = timdb.velps.get_document_velps(doc_id)
    # print(velp_data) # TODO Just for checking, delete later
    return jsonResponse(velp_data)


@velps.route("/<document_id>/getvelpgrouplocations", methods=['GET'])
def get_velp_group_locations(document_id: int) -> str:
    timdb = getTimDb()
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    location_data = timdb.velp_groups.get_velp_groups_in_assessment_area(doc_id)
    return jsonResponse(location_data)


@velps.route("/createvelpgroup", methods=['GET'])
def create_velp_group():
    # TODO: Remove comments, kill test data from below
    # TODO: method back to POST
    '''
    json_data = request.get_json()
    # .get returns null instead of throwing if data is missing.
    velp_group_name = json_data.get('name')
    owner_group_id = json_data.get('owner')
    root_path = json_data.get('root_path')
    valid_until = json_data.get('valid_until')
    doc_id = json_data.get('doc_id')
    personal_group = json_data.get('personal_group')
    '''

    velp_group_name = "Kana"
    owner_group_id = 3
    valid_until = None
    root_path = "users/josalatt/testikansio"
    doc_id = 89
    personal_group = True
    timdb = getTimDb()

    doc_name_info = timdb.documents.get_names(doc_id)
    doc_name = doc_name_info[0]['name']

    # Create a new velp group / document in users/username/velp groups folder
    if personal_group is True:
        user = getCurrentUserName()
        user_group_id = getCurrentUserGroup()
        print(user_group_id)
        user_velp_path = timdb.folders.check_personal_velp_folder(user, user_group_id)
        print(user_velp_path)
        new_group_path = user_velp_path + "/" + velp_group_name
        group_exists = timdb.documents.resolve_doc_id_name(new_group_path)
        if group_exists is None:
            new_group = timdb.documents.create(new_group_path, user_group_id)
            new_group_id = new_group.doc_id
            velp_group_id = timdb.velp_groups.create_velp_group2(velp_group_name, user_group_id, new_group_id, valid_until)
        else:
            abort(400, "Velp group with same name and location exists already.")

        return jsonResponse(velp_group_id)


    # Gives path to either velp groups or velp groups/document name folder
    velps_folder_path = timdb.folders.check_velp_group_folder_path(root_path, owner_group_id, doc_name)
    new_group_path = velps_folder_path + "/" + velp_group_name

    group_exists = timdb.documents.resolve_doc_id_name(new_group_path) # Check name so no duplicates are made
    if group_exists is None:
        new_group = timdb.documents.create(new_group_path, owner_group_id)
        new_group_id = new_group.doc_id
        velp_group_id = timdb.velp_groups.create_velp_group2(velp_group_name, owner_group_id, new_group_id, valid_until)
        timdb.velp_groups.insert_group_to_document(velp_group_id, doc_id)
    else:
        abort(400, "Velp group with same name and location exists already.")

    return jsonResponse(velp_group_id)


@velps.route("/<document_id>/labels", methods=['GET'])
def get_labels(document_id: int) -> 'str':
    timdb = getTimDb()
    try:
        doc_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    # Todo select language.
    label_data = timdb.velps.get_document_velp_label_content(doc_id)
    return jsonResponse(label_data)

@velps.route("/addvelp", methods=['POST'])
def add_velp():
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

    print(velp_content)
    print(velp_labels)
    print(velp_groups)

    default_points = float(default_points) if default_points is not None else None
    icon_id = int(icon_id) if icon_id is not None else None

    timdb = getTimDb()
    current_user_id = getCurrentUserId()

    for group in velp_groups:
        if timdb.users.has_edit_access(current_user_id, group) is False:
            abort(400, "No edit access for velp group: " + timdb.velp_groups.get_velp_group_name(group))

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
        timdb.velp_groups.add_velp_to_group(new_velp_id, 1)
    # Todo write logic that decides where the velp should go.
    return jsonResponse(new_velp_id)


@velps.route("/updatevelp", methods=['POST'])
def update_velp():
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
    current_user_id = getCurrentUserId()
    edit_access = False

    # Check
    for group in velp_groups:
        if timdb.users.has_edit_access(current_user_id, group) is True:
            edit_access = True
            break
    if edit_access is False:
        abort(400, "No edit access to velp via any velp group.")

    old_content = timdb.velps.get_latest_velp_version(velp_id, language_id)
    old_labels = timdb.velps.get_velp_label_ids(velp_id)
    if old_content != new_content:
        # Todo this does not really work correctly, now any update to any language creates a new version, and we can not
        # produce different contents with the same version but different language.

        version_id = timdb.velps.create_velp_version(velp_id)
        timdb.velps.create_velp_content(version_id, language_id, new_content)
    if old_labels != new_labels:
        timdb.velps.update_velp_labels(velp_id, new_labels)
    timdb.velps.update_velp(velp_id, default_points, icon_id)


@velps.route("/addlabel", methods=["POST"])
def add_label():
    # language_id = request.args.get('language_id')
    json_data = request.get_json()
    try:
        content = json_data['content']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    language_id = json_data.get('language_id')
    language_id = "FI" if language_id is None else language_id

    timdb = getTimDb()
    label_id = timdb.velps.create_label(language_id, content)

    return jsonResponse(label_id)


@velps.route("/create_default_velp_group", methods=["GET"])
def create_default_velp_group():
    """

    :return:
    """

    timdb = getTimDb()
    owner_group_id = 3  # Korppi users
    root_path = "users/josalatt/testikansio"
    doc_name = "test2"

    # Get velp group folder path and if necessary, creates those folders
    velps_folder_path = timdb.folders.check_velp_group_folder_path(root_path, owner_group_id, doc_name)

    velp_groups = timdb.documents.get_documents_in_folder(velps_folder_path)
    default_velp_group = False
    default_group_name = doc_name + "_default"
    # Check through all documents in velp group folder to check if default group exists
    if velp_groups is not None:
        for group in velp_groups:
            print(group)
            if group['name'] == velps_folder_path + "/" + default_group_name:
                default_velp_group = True

    # If default didn't exists yet (or there were no documents / velp groups to start with), create one
    if default_velp_group is False:
        default_group_path = velps_folder_path + "/" + default_group_name
        new_group = timdb.documents.create(default_group_path, owner_group_id)
        new_group_id = new_group.doc_id
        doc_id = timdb.velp_groups.create_default_velp_group2(default_group_name, owner_group_id, new_group_id)
        velp_groups = timdb.documents.get_documents_in_folder(velps_folder_path)
        print("Default group didn't exist, created one with id: " + str(new_group_id))

    return jsonResponse(velp_groups)


@velps.route("/get_velp_groups")
def get_velp_groups():
    """Returns all velp groups found from tree from document to root and from users own velp folder

    Checks document's own velp group folder first, then default velp group folders going up all the
    way to root. Doesn't branch side ways or down, only checks parents. After root has been reached,
    finally checks users own velp group folder.

    Checks that user has minimum of view right for velp groups.
    :return: List of document / velp group information of found hits.
    """
    json_data = request.get_json()
    '''
    current_path = json_data.get('root_path')

    '''
    current_path = "users/josalatt/testikansio"
    username = getCurrentUserName()
    doc_name = "testi1"
    personal_velps_path = "users/" + username + "/velp groups"
    timdb = getTimDb()
    velp_groups = []

    found_velp_groups = timdb.documents.get_documents_in_folder(current_path + "/velp groups/" + doc_name)
    print(found_velp_groups)
    print(current_path + "/velp groups/" + doc_name)
    for v in found_velp_groups:
        if timdb.users.has_view_access(getCurrentUserId(), timdb.documents.get_document_id(v['name'])):
                velp_groups.append(v)

    while True:
        if current_path != '':
            found_velp_groups = timdb.documents.get_documents_in_folder(current_path + '/velp groups')
        else:
            found_templates = timdb.documents.get_documents_in_folder('velp groups')
        print(found_velp_groups)
        print(current_path)
        for v in found_velp_groups:
            if timdb.users.has_view_access(getCurrentUserId(), timdb.documents.get_document_id(v['name'])):
                velp_groups.append(v)
        if current_path == '':
            break
        current_path, _ = timdb.folders.split_location(current_path)

    found_velp_groups = timdb.documents.get_documents_in_folder(personal_velps_path)
    print(found_velp_groups)
    print(personal_velps_path)
    for v in found_velp_groups:
        if timdb.users.has_view_access(getCurrentUserId(), timdb.documents.get_document_id(v['name'])):
                velp_groups.append(v)

    result = [dict(t) for t in set(tuple(d.items()) for d in velp_groups)] # Remove possible doubles

    return jsonResponse(result)