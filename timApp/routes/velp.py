from flask import Blueprint
from .common import *
import copy


velps = Blueprint('velps',
                    __name__,
                    url_prefix='')

@velps.route("/<document_id>/<paragraph_id>/velps", methods=['GET'])
def get_velps(document_id: int, paragraph_id: str) -> 'str':
    timdb = getTimDb()
    #Todo Somehow communicate the language string for the get_document_velps function.
    velp_data_raw = timdb.velps.get_document_velps(int(document_id))
    velp_data = """[
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
    label_data_raw = timdb.labels.get_document_velp_label_ids(int(document_id))
    print(velp_data_raw)
    print(label_data_raw)

    # Add try catch in case there are no labels at all
    # Add null label lists as well?

    velp_id = label_data_raw[0]['velp_id']
    list_help = []
    label_dict = {}
    for i in range(len(label_data_raw)):
        next_id = label_data_raw[i]['velp_id']
        if next_id != velp_id:
            label_dict[velp_id] = copy.deepcopy(list_help)
            velp_id = next_id
            del list_help[:]
            list_help.append(label_data_raw[i]['labels'])
        else:
            list_help.append(label_data_raw[i]['labels'])
            if i == len(label_data_raw) - 1:
                label_dict[velp_id] = copy.deepcopy(list_help)

    for i in range(len(velp_data_raw)):
        searchID = velp_data_raw[i]['id']
        if searchID in label_dict:
            velp_data_raw[i]['labels'] = label_dict[searchID]

    # Try catch should end here

    print(velp_data_raw)


    return jsonResponse(velp_data_raw)

@velps.route("/<document_id>/labels", methods=['GET'])
def get_labels(document_id: int)-> 'str':
    timdb = getTimDb()
    #Todo select language.
    #I have no idea why labels is not a member of timdb.
    label_data = timdb.labels.get_document_velp_labels(int(document_id))
    return jsonResponse(label_data)