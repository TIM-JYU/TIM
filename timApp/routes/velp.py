from flask import Blueprint
from .common import *

velps = Blueprint('velps',
                  __name__,
                  url_prefix='')


@velps.route("/<document_id>/<paragraph_id>/velps", methods=['GET'])
def get_velps(document_id: int, paragraph_id: str) -> str:
    timdb = getTimDb()
    default_exists = timdb.velp_groups.check_default_group_exists(document_id)
    doc_id = int(document_id)       # Make sure document_id is int
    if default_exists is False:     # Create new default velp group if one does not exist yet
        default_group_name = timdb.documents.get_first_document_name(doc_id)
        new_default_id = timdb.velp_groups.create_default_velp_group(default_group_name, 1, None)
        timdb.velp_groups.insert_group_to_document(int(new_default_id), doc_id)
        print("Created a new default velp group, ID: " + str(new_default_id) \
              + ", name: " + default_group_name + ", document ID: " + document_id)
    # Todo Somehow communicate the language string for the get_document_velps function.

    velp_data = timdb.velps.get_document_velps(doc_id)
    print(velp_data) # Just for checking, delete later
    return jsonResponse(velp_data)

@velps.route("/createvelpgroup", methods=['GET'])
def create_velp_group():
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
    # Todo select language.
    label_data = timdb.velps.get_document_velp_label_content(int(document_id))
    return jsonResponse(label_data)

@velps.route("/copygrupa", methods=['GET'])
def copy_velp_group():
    timdb = getTimDb()
    # Todo select language.
    timdb.velp_groups.copy_velp_group(1, 4)
    return "ASDASD"


# TODO remove this
"""
@velps.route("/addvelp", methods=['GET'])
def add_velp(velp_content: str = "MOIMOI", default_points: int = -5.0, language_id: str = "FI",
             icon_id: int = None, valid_until: str = None, velp_labels: [] = [2]):

    timdb = getTimDb()
    current_user_id = getCurrentUserId()

    latest_velp = timdb.velps.create_velp(current_user_id, default_points, icon_id, valid_until)
    latest_version = timdb.velps.create_velp_version(latest_velp)
    timdb.velps.create_velp_content(latest_version, language_id, velp_content)
    for i in range(len(velp_labels)):
        timdb.velp_groups.add_velp_to_group(latest_velp, velp_labels[i])

    return str(latest_version)
"""


@velps.route("/addvelp", methods=['POST'])
def add_velp():

    json_data = request.get_json()
    # .get returns null instead of throwing if data is missing.
    velp_content = json_data.get('content')
    default_points = json_data.get('points')
    language_id = json_data.get('language_id')
    icon_id = json_data.get('icon_id')
    valid_until = json_data.get('valid_until')
    velp_labels = json_data.get('labels')
    velp_groups = json_data.get('velp_groups')

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
            timdb.velp_groups.add_velp_to_group(new_velp_id, group_id)
    else:
        timdb.velp_groups.add_velp_to_group(new_velp_id, 1)
    # Todo write logic that decides where the velp should go.
    return jsonResponse(new_velp_id)


@velps.route("/updatevelp", methods=['POST'])
def update_velp():
    json_data = request.get_json()
    velp_id = json_data.get('id')
    new_content = json_data.get('content')
    default_points = json_data.get('points')
    language_id = json_data.get('language_id')
    icon_id = json_data.get('icon_id')
    new_labels = json_data.get('labels')
    timdb = getTimDb()
    old_content = timdb.velps.get_latest_velp_version(velp_id, language_id)
    old_labels=timdb.velps.get_velp_label_ids(velp_id)
    if old_content != new_content:
        #Todo this does not really work correctly, now any update to any language creates a new version, there can not
        #be different contents with the same version but different language.
        version_id=timdb.velps.create_velp_version(velp_id)
        timdb.velps.create_velp_content(version_id,language_id,new_content)
    if old_labels != new_labels:
        timdb.velps.update_velp_labels(velp_id,new_labels)
    timdb.velps.update_velp(velp_id, default_points, icon_id)


@velps.route("/addlabel", methods=["POST"])
def add_label():
    # language_id = request.args.get('language_id')
    json_data = request.get_json()
    language_id = "FI"
    content = json_data['content']

    timdb = getTimDb()
    label_id = timdb.velps.create_label(language_id, content)

    return jsonResponse(label_id)
