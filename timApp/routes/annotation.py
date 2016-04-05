from flask import Blueprint
from .common import *
from assesment_area import assessment_area_from_document

annotations = Blueprint('annotations',
                        __name__,
                        url_prefix='')


# This and add_comment should be connected to the ui so we can get rid of the
# default parameters and strange return values.

@annotations.route("/addannotation", methods=['GET'])
def add_annotation(velp_id: int = 1, points: float = 1.5, place_start: int = 1, place_end: int = 2,
                   document_id: int = 1, paragraph_id: str = '1', answer_id: int = None,
                   icon_id: int = None):
    timdb = getTimDb()
    annotator_id = getCurrentUserId()
    velp_version = timdb.velps.get_latest_velp_version(velp_id)
    timdb.annotations.create_annotation(velp_version, points, place_start, place_end, annotator_id,
                                        document_id, paragraph_id, icon_id, answer_id)
    return "Added an annotation"


@annotations.route("/addannotationcomment", methods=['GET'])
def add_comment(annotation_id: int = 1, content: str = "Joo joo!") -> str:
    timdb = getTimDb()
    commenter_id = getCurrentUserId()
    timdb.annotations_comments.add_comment(annotation_id, commenter_id, content)
    return "Added a comment."


@annotations.route("/<document_id>/<paragraph_id>/annotations", methods=['GET'])
def get_annotations(document_id: int, paragraph_id: str) -> str:
    timdb = getTimDb()
    results = timdb.annotations.get_annotations(int(document_id), paragraph_id)
    return jsonResponse(results)
