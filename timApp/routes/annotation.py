from flask import Blueprint
from .common import *

annotations = Blueprint('annotations',
                    __name__,
                    url_prefix='')

# Does not work yet!
@annotations.route("/addannotation2", methods=['GET'])
def add_annotation(velp_id: int = 1, points: float = 1.5, place_start: int = 1, place_end: int = 2,
                   document_id: int = None, paragraph_id: int = None, answer_id: int = None,
                   icon_id: int = None):
    timdb = getTimDb()
    annotator_id = getCurrentUserId()
    velp_version = timdb.velps.get_latest_velp_version(velp_id)
    timdb.annotations.create_annotation(velp_version, points, place_start, place_end, annotator_id,
                                                 icon_id, document_id, paragraph_id, answer_id)

    return "Added annotation"