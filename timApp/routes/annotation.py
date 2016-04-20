from flask import Blueprint
from .common import *

annotations = Blueprint('annotations',
                        __name__,
                        url_prefix='')


# TODO connect the routes in this file to the ui.
@annotations.route("/addannotation", methods=['POST'])
def add_annotation() -> str:
    json_data = request.get_json()
    # first get the non-optional arguments and abort if there is missing data.
    try:
        velp_id = json_data['velp']
        document_id = json_data['doc_id']
        coordinates = json_data['coord']
        start = coordinates['start']
        end = coordinates['end']

        offset_start = start['offset']
        element_path_start = str(start['el_path'])

        offset_end = end['offset']
        element_path_end = str(end['el_path'])
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])

    # .get() returns None if there is no data instead of throwing.
    points = json_data.get('points')
    icon_id = json_data.get('icon_id')

    # Now ensure that the data is good for either an answer or a paragraph target.
    answer_id = json_data.get('answer_id')
    if answer_id is None:
        try:
            paragraph_id_start = start['par_id']
            hash_start = start['t']
            paragraph_id_end = end['par_id']
            hash_end = end['t']
        except KeyError as e:
            abort(400, "Missing data: " + e.args[0])
    else:
        paragraph_id_start = start.get('par_id')
        hash_start = start.get('t')
        paragraph_id_end = end.get('par_id')
        hash_end = start.get('t')
        some_paragraph_data_present = paragraph_id_start is not None
        some_paragraph_data_present = hash_start is not None or some_paragraph_data_present
        some_paragraph_data_present = paragraph_id_end is not None or some_paragraph_data_present
        some_paragraph_data_present = hash_end is not None or some_paragraph_data_present
        if some_paragraph_data_present:
            abort(400, "Both answer_id and paragraph data present.")

    timdb = getTimDb()
    annotator_id = getCurrentUserId()
    velp_version = timdb.velps.get_latest_velp_version(velp_id)
    new_id = timdb.annotations.create_annotation(velp_version, points, annotator_id, document_id, paragraph_id_start,
                                                 paragraph_id_end, offset_start, offset_end, hash_start, hash_end,
                                                 element_path_start, element_path_end, None, icon_id, answer_id)
    return jsonResponse(new_id)


@annotations.route("/addannotationcomment", methods=['POST'])
def add_comment() -> str:
    annotation_id = request.args.get('annotation_id')
    content = request.args.get('content')
    timdb = getTimDb()
    commenter_id = getCurrentUserId()
    new_id = timdb.annotations.add_comment(annotation_id, commenter_id, content)
    return jsonResponse(new_id)


@annotations.route("/<document_id>/annotations", methods=['GET'])
def get_annotations(document_id: int, paragraph_id: str) -> str:
    timdb = getTimDb()
    results = timdb.annotations.get_annotations_in_document(int(document_id))
    return jsonResponse(results)


# TODO decide whether we should instead return comments for just one annotation, instead of returning everything at
# once, like here.
@annotations.route("/<document_id>/comments", methods=['GET'])
def get_comments(document_id: int) -> str:
    timdb = getTimDb()
    results = timdb.annotations_comments.get_comments(int(document_id))
    return jsonResponse(results)
