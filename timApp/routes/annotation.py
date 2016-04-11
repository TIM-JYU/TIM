from flask import Blueprint
from .common import *

annotations = Blueprint('annotations',
                        __name__,
                        url_prefix='')


# TODO connect the routes in this file to the ui.
# TODO save element_number also.

@annotations.route("/addannotation", methods=['POST'])
def add_annotation():
    velp_id = request.args.get('velp_id')
    points = request.args.get('points')
    place_start = request.args.get('place_start')
    place_end = request.args.get('place_end')
    document_id = request.args.get('document_id')
    paragraph_id = request.args.get('paragraph_id')
    answer_id = request.args.get('answer_id')
    icon_id = request.args.get('icon_id')
    element_number = request.args.get('element_number')

    timdb = getTimDb()
    annotator_id = getCurrentUserId()
    velp_version = timdb.velps.get_latest_velp_version(velp_id)
    timdb.annotations.create_annotation(velp_version, points, place_start, place_end, annotator_id,
                                        document_id, paragraph_id, element_number, icon_id, answer_id)
    return "Added an annotation"


@annotations.route("/addannotationcomment", methods=['POST'])
def add_comment():
    annotation_id = request.args.get('annotation_id')
    content = request.args.get('content')
    timdb = getTimDb()
    commenter_id = getCurrentUserId()
    timdb.annotations_comments.add_comment(annotation_id, commenter_id, content)
    return "Added a comment."


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
