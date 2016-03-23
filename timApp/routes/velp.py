from flask import Blueprint
from .common import *


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
    return jsonResponse(velp_data_raw)

@velps.route("/<document_id>/labels", methods=['GET'])
def get_labels(document_id: int)-> 'str':
    timdb = getTimDb()
    #Todo select language.
    #I have no idea why labels is not a member of timdb.
    label_data = timdb.labels.get_document_velp_labels(int(document_id))
    return jsonResponse(label_data)