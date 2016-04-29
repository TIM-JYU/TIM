from flask import Blueprint
from .common import *

velps = Blueprint('velps',
                  __name__,
                  url_prefix='')


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
        default_velp_group_id = timdb.velp_groups.create_default_velp_group(default_group_name, 1, None)
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
        new_default_id = timdb.velp_groups.create_default_velp_group(default_group_name, 1, None)
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


@velps.route("/createvelpgroup", methods=['POST'])
def create_velp_group():
    json_data = request.get_json()
    # .get returns null instead of throwing if data is missing.
    velp_group_name = json_data.get('name')
    owner_group_id = json_data.get('owner')
    valid_until = json_data.get('valid_until')

    velp_group_name = "Kana"
    owner_group_id = 1
    valid_until = None
    timdb = getTimDb()
    velp_group_id = timdb.velp_groups.create_velp_group(velp_group_name, owner_group_id, valid_until)
    timdb.velp_groups.insert_group_to_document(velp_group_id, 1)
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


@velps.route("/copygrupa", methods=['GET'])
def copy_velp_group():
    timdb = getTimDb()
    # Todo select language.
    timdb.velp_groups.copy_velp_group(1, 4)
    return "ASDASD"


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
    except KeyError as e:
        abort(400, "Missing data " + e.args[0])
    if not new_content:
        abort(400, "Empty content string.")

    default_points = json_data.get('points')
    icon_id = json_data.get('icon_id')
    new_labels = json_data.get('labels')
    timdb = getTimDb()
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

# TODO Better name needed here, just testing for now
@velps.route("/velpabc", methods=["GET"])
def velpabc():
    timdb = getTimDb()
    owner_group_id = 3  # Korppi users
    root_path = "users/josalatt/testikansio"
    velps_folder_path = root_path + "/velps"
    folders = timdb.folders.get_folders(root_path)
    velps_folder = False;
    # Check if velps folder exist
    for folder in folders:
        if folder['name'] == "velps":
            print("ASD")
            velps_folder = True;
    #
    if velps_folder is False:
        new_block = timdb.folders.create(velps_folder_path, owner_group_id)
        print("Created new folder, id: " + str(new_block))


    # TODO fix below, get_documents_in_folder is broken
    velp_groups = timdb.documents.get_documents_in_folder(velps_folder_path)
    if velp_groups is not None:
        for dadada in velp_groups:
            asd = 1
        asd = 1
    else:
        default_group = velps_folder_path + "/" + "testi1" + "_default"
        timdb.documents.create(default_group, owner_group_id)



    return jsonResponse(velp_groups)
