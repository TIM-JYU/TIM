from flask import Blueprint
from .common import *


velps = Blueprint('velps',
                    __name__,
                    url_prefix='')

@velps.route("/<document_id>/<paragraph_id>/velps", methods=['GET'])
def get_velps(document_id: str, paragraph_id: str):
    timdb = getTimDb()

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
    return velp_data
