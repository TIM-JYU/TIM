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
    if len(velp_data):
        print("sdfdsf")
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
    jsondata = request.get_json()

    velp_content = jsondata['content']
    default_points = jsondata['points']
    language_id = jsondata['language_id']
    icon_id = jsondata['icon_id']
    valid_until = jsondata['valid_until']
    velp_labels = jsondata['labels']
    velp_groups = jsondata['velp_groups']


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


@velps.route("/addlabel", methods=["POST"])
def add_label():
    #language_id = request.args.get('language_id')
    jsondata = request.get_json()
    language_id = "FI"
    content = jsondata['content']

    timdb = getTimDb()
    label_id = timdb.velps.create_label(language_id, content)

    return jsonResponse(label_id)
