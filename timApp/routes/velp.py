from flask import Blueprint
from .common import *
import copy

velps = Blueprint('velps',
                  __name__,
                  url_prefix='')


@velps.route("/<document_id>/<paragraph_id>/velps", methods=['GET'])
def get_velps(document_id: int, paragraph_id: str) -> str:
    timdb = getTimDb()
    # Todo Somehow communicate the language string for the get_document_velps function.
    velp_data = timdb.velps.get_document_velps(int(document_id))
    test_velp_data = """[
        {
        "tags": [0],
        "used": 3,
        "id": 0,
        "points": -1,
        "content": "Sentinel missing"
        },
        {
        "tags": [0],
        "used": 0,
        "id": 1,
        "points": -1,
        "content": "Does not terminate with sentinel"
        }]"""
    label_data = timdb.labels.get_document_velp_label_ids(int(document_id))
    print(velp_data)
    print(label_data)

    # Add try catch in case there are no labels at all
    # Add null label lists as well?

    velp_id = label_data[0]['velp_id']
    list_help = []
    label_dict = {}
    for i in range(len(label_data)):
        next_id = label_data[i]['velp_id']
        if next_id != velp_id:
            label_dict[velp_id] = copy.deepcopy(list_help)
            velp_id = next_id
            del list_help[:]
            list_help.append(label_data[i]['labels'])
        else:
            list_help.append(label_data[i]['labels'])
        if i == len(label_data) - 1:
            label_dict[velp_id] = copy.deepcopy(list_help)

    for i in range(len(velp_data)):
        searchID = velp_data[i]['id']
        if searchID in label_dict:
            velp_data[i]['labels'] = label_dict[searchID]

    # Try catch should end here


    return jsonResponse(velp_data)


@velps.route("/<document_id>/labels", methods=['GET'])
def get_labels(document_id: int) -> 'str':
    timdb = getTimDb()
    # Todo select language.
    label_data = timdb.labels.get_document_velp_label_content(int(document_id))
    return jsonResponse(label_data)


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


# Might replace /addvelp route as the actual route.
@velps.route("/addvelp", methods=['POST'])
def add_velp2():
    velp_content = request.form.get('content')
    default_points = request.form.get('points')
    language_id = request.form.get('language_id')
    icon_id = request.form.get('icon_id')
    valid_until = request.form.get('valid_until')
    velp_labels = request.form.get('labels')

    default_points = float(default_points) if default_points is not None else None
    icon_id = int(icon_id) if icon_id is not None else None

    timdb = getTimDb()
    current_user_id = getCurrentUserId()

    new_velp = timdb.velps.create_velp(current_user_id, default_points, icon_id, valid_until)
    new_version = timdb.velps.create_velp_version(new_velp)
    timdb.velps.create_velp_content(new_version, language_id, velp_content)
    if velp_labels is not None:
        timdb.labels.add_labels_to_velp(new_velp,velp_labels)
    #Todo write logic that decides where the velp should go.
    timdb.velp_groups.add_velp_to_group(new_velp, 1)
    return str(new_velp)