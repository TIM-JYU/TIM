from flask import Blueprint
from .common import *


velps = Blueprint('velps',
                    __name__,
                    url_prefix='')

@velps.route("/<document_id>/<paragraph_id>/velps", methods=['GET'])
def get_velps(document_id: int, paragraph_id: str):
    timdb = getTimDb()
    velp_data_raw = timdb.velps.get_document_velps(int(document_id))
    velp_data ="""
      [{
    "tags": [
      0
    ],
    "used": 3,
    "id": 0,
    "points": -1,
    "content": "Sentinel missing"
  },
  {
    "tags": [
      0
    ],
    "used": 0,
    "id": 1,
    "points": -1,
    "content": "Does not terminate with sentinel"
  }]"""
    return str(velp_data_raw)
