from flask import Blueprint
from .common import *
from timdb.annotations import Annotations

annotations = Blueprint('annotations',
                        __name__,
                        url_prefix='')


# TODO connect the routes in this file to the ui.
@annotations.route("/add_annotation", methods=['POST'])
def add_annotation() -> str:
    json_data = request.get_json()
    print(json_data)
    timdb = getTimDb()

    # first get the non-optional arguments and abort if there is missing data.
    try:
        velp_id = json_data['velp']
        visible_to = timdb.annotations.AnnotationVisibility(json_data['visible_to'])
        document_id = json_data['doc_id']
        coordinates = json_data['coord']
        start = coordinates['start']
        end = coordinates['end']

        offset_start = start['offset']
        depth_start = start['depth']
        node_start = start['node']

        element_path_start = start['el_path']
        if type(element_path_start) is not list:
            raise TypeError(str(element_path_start))
        if any(type(i) is not int for i in element_path_start):
            raise TypeError(str(element_path_start))
        element_path_start = str(element_path_start)

        offset_end = end['offset']
        depth_end = end['depth']
        node_end = end['node']

        element_path_end = end['el_path']
        if type(element_path_end) is not list:
            raise TypeError(str(element_path_end))
        if any(type(i) is not int for i in element_path_end):
            raise TypeError(str(element_path_end))
        element_path_end = str(element_path_end)

    except KeyError as e:  # one of the json_data['foo'] fails
        abort(400, "Missing data: " + e.args[0])
    except TypeError as e:  # one of the element paths is not a list of integers
        abort(400, "Malformed element path. " + e.args[0])
    except ValueError as e:  # visible_to could not be casted to the enum used.
        abort(400, e.args[0])

    # .get() returns None if there is no data instead of throwing.
    points = json_data.get('points')
    icon_id = json_data.get('icon_id')
    answer_id = json_data.get('answer_id')

    try:
        paragraph_id_start = start['par_id']
        hash_start = start['t']
        paragraph_id_end = end['par_id']
        hash_end = end['t']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    verifyLoggedIn()
    annotator_id = getCurrentUserId()
    velp_version = timdb.velps.get_latest_velp_version(velp_id)
    new_id = timdb.annotations.create_annotation(velp_version, visible_to, points, annotator_id, document_id,
                                                 paragraph_id_start, paragraph_id_end, offset_start, node_start,
                                                 depth_start, offset_end, node_end, depth_end, hash_start, hash_end,
                                                 element_path_start, element_path_end, None, icon_id, answer_id)
    return jsonResponse(new_id)


@annotations.route("/update_annotation", methods=['POST'])
def update_annotation() -> str:
    verifyLoggedIn()
    user_id = getCurrentUserId()
    json_data = request.get_json()
    try:
        annotation_id = json_data['annotation_id']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    visible_to = json_data.get('visible_to')
    points = json_data.get('points')
    timdb = getTimDb()
    # Get values from the database to fill in unchanged new values.
    new_values = timdb.annotations.get_annotation(annotation_id)
    if not new_values:
        abort(404, "No such annotation.")
    new_values = new_values[0]
    # Todo panic if there is more than one item in the list.
    if not new_values['annotator_id'] == user_id:
        abort(403, "You are not the annotator.")
    if visible_to:
        try:
            visible_to = Annotations.AnnotationVisibility(visible_to)
        except ValueError as e:
            abort(400, "Visibility should be 1, 2, 3 or 4.")
        new_values['visible_to'] = visible_to
    new_values['points'] = points
    timdb.annotations.update_annotation(new_values['id'], new_values['velp_version_id'], new_values['visible_to'],
                                        new_values['points'], new_values['icon_id'])
    return ""


@annotations.route("/invalidate_annotation", methods=['POST'])
def invalidate_annotation() -> str:
    json_data = request.get_json()
    try:
        annotation_id = json_data['annotation_id']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    timdb = getTimDb()
    annotation = timdb.annotations.get_annotation(annotation_id)
    if not annotation:
        abort(404, "No such annotation.")
    annotation = annotation[0]
    verifyLoggedIn()
    user_id = getCurrentUserId()
    if not annotation['annotator_id'] == user_id:
        abort(403, "You are not the annotator.")
    timdb.annotations.invalidate_annotation(annotation_id)
    return ""


@annotations.route("/add_annotation_comment", methods=['POST'])
def add_comment() -> str:
    json_data = request.get_json()
    try:
        annotation_id = json_data['annotation_id']
        content = json_data['content']
    except KeyError as e:
        abort(400, "Missing data: " + e.args[0])
    # Todo maybe check that content isn't an empty string
    timdb = getTimDb()
    verifyLoggedIn()
    commenter_id = getCurrentUserId()
    timdb.annotations.add_comment(annotation_id, commenter_id, content)
    return jsonResponse(timdb.users.get_user(commenter_id))


@annotations.route("/<document_id>/get_annotations", methods=['GET'])
def get_annotations(document_id: int) -> str:
    try:
        document_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    timdb = getTimDb()
    user_id = getCurrentUserId()
    if not timdb.documents.exists(document_id):
        abort(404, "No such document.")
    if not timdb.users.has_view_access(user_id, document_id):
        abort(403, "View access required.")
    user_has_see_answers = timdb.users.has_seeanswers_access(user_id, document_id)
    user_has_teacher = timdb.users.has_teacher_access(user_id, document_id)
    user_has_owner = timdb.users.user_is_owner(user_id, document_id)

    results = timdb.annotations.get_annotations_with_comments_in_document(getCurrentUserId(), user_has_see_answers,
                                                                          user_has_teacher, user_has_owner, document_id)
    response = jsonResponse(results)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response


# TODO Outdated routes / methods

# TODO decide whether we should instead return comments for just one annotation, instead of returning everything at
# once, like here.
@annotations.route("/<document_id>/comments", methods=['GET'])
def get_comments(document_id: int) -> str:
    timdb = getTimDb()
    try:
        document_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    if not timdb.documents.exists(document_id):
        abort(404, "No such document.")
    results = timdb.annotations.get_comments_in_document(document_id)
    return jsonResponse(results)


@annotations.route("/<document_id>/get_comments")
def get_comments2(document_id: int):
    timdb = getTimDb()
    doc_id = int(document_id)
    comments = timdb.annotations.get_annotations_with_comments_in_document(doc_id)
    return jsonResponse(comments)


# Todo maybe check that the document in question actually exists and return on error if not.
@annotations.route("/<document_id>/annotationsorig", methods=['GET'])
def get_annotationsorig(document_id: int) -> str:
    try:
        document_id = int(document_id)
    except ValueError as e:
        abort(400, "Document_id is not a number.")
    timdb = getTimDb()
    user_id = getCurrentUserId()
    if not timdb.documents.exists(document_id):
        abort(404, "No such document.")
    if not timdb.users.has_view_access(user_id, document_id):
        abort(403, "View access required.")
    user_teacher = timdb.users.has_teacher_access(user_id, document_id)
    user_owner = timdb.users.user_is_owner(user_id, document_id)

    results = timdb.annotations.get_annotations_in_document(getCurrentUserId(), user_teacher, user_owner,
                                                            int(document_id))
    response = jsonResponse(results)
    response.headers['Cache-Control'] = 'no-store, no-cache, must-revalidate'
    return response
