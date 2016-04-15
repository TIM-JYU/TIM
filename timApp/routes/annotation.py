from flask import Blueprint
from .common import *

annotations = Blueprint('annotations',
                        __name__,
                        url_prefix='')


# TODO connect the routes in this file to the ui.
# TODO save element_number also.

@annotations.route("/addannotation", methods=['POST'])
def add_annotation() -> str:
    json_data = request.get_json()

    velp_id = json_data['velp_id']
    points = json_data['points']
    place_start = json_data['place_start']
    place_end = json_data['place_end']
    document_id = json_data['document_id']
    paragraph_id = json_data['paragraph_id']
    answer_id = json_data['answer_id']
    icon_id = json_data['icon_id']
    element_number = json_data['element_number']

    timdb = getTimDb()
    annotator_id = getCurrentUserId()
    velp_version = timdb.velps.get_latest_velp_version(velp_id)
    new_id = timdb.annotations.create_annotation(velp_version, points, place_start, place_end, annotator_id,
                                                 document_id, paragraph_id, element_number, icon_id, answer_id)
    return jsonResponse(new_id)


@annotations.route("/addannotationcomment", methods=['POST'])
def add_comment() -> str:
    annotation_id = request.args.get('annotation_id')
    content = request.args.get('content')
    timdb = getTimDb()
    commenter_id = getCurrentUserId()
    new_id=timdb.annotations_comments.add_comment(annotation_id, commenter_id, content)
    return jsonResponse(new_id)


@annotations.route("/<document_id>/<paragraph_id>/annotations", methods=['GET'])
def get_annotations(document_id: int, paragraph_id: str) -> str:
    timdb = getTimDb()
    results = timdb.annotations.get_annotations(int(document_id), paragraph_id)
    return jsonResponse(results)


# TODO decide whether we should instead return comments for just one annotation, instead of returning everything at
# once, like here.
@annotations.route("/<document_id>/<paragraph_id>/comments", methods=['GET'])
def get_comments(document_id: int, paragraph_id: str) -> str:
    timdb = getTimDb()
    results = timdb.annotations_comments.get_comments(int(document_id), paragraph_id)
    return jsonResponse(results)
