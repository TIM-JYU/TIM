"""The module handles the main logic related to annotations. This includes adding, modifiying and deleting annotations
as well as adding comments to the annotations. The module also retrieves the annotations to the document.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""
import json
import re
from dataclasses import dataclass, field
from typing import Optional, List

from flask import Blueprint

from timApp.answer.routes import verify_answer_access
from timApp.auth.accesshelper import verify_logged_in, has_teacher_access, \
    get_doc_or_abort, verify_view_access, AccessDenied
from timApp.auth.sessioninfo import get_current_user_object
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import RouteException, use_model
from timApp.util.flask.responsehelper import json_response, ok_response, no_cache_json_response
from timApp.util.utils import get_current_time
from timApp.velp.annotation_model import Annotation, AnnotationPosition
from timApp.velp.annotations import AnnotationVisibility, get_annotations_with_comments_in_document, \
    set_annotation_query_opts
from timApp.velp.velp_models import AnnotationComment
from timApp.velp.velps import get_latest_velp_version

annotations = Blueprint('annotations',
                        __name__,
                        url_prefix='')


@dataclass
class AddAnnotationModel:
    velp_id: int
    doc_id: int
    coord: AnnotationPosition
    visible_to: AnnotationVisibility = field(metadata={'by_value': True})
    points: Optional[float]
    color: Optional[str] = None
    icon_id: Optional[int] = None
    answer_id: Optional[int] = None
    draw_data: Optional[List[dict]] = None


@annotations.route("/add_annotation", methods=['post'])
@use_model(AddAnnotationModel)
def add_annotation(m: AddAnnotationModel):
    """Adds a new annotation.
    """
    d = get_doc_or_abort(m.doc_id)
    verify_view_access(d)
    color = m.color
    validate_color(color)
    annotator = get_current_user_object()
    velp_version_id = get_latest_velp_version(m.velp_id).version_id

    if m.answer_id:
        verify_answer_access(m.answer_id, get_current_user_object().id, require_teacher_if_not_own=True)
    ann = Annotation(
        velp_version_id=velp_version_id,
        visible_to=m.visible_to.value,
        points=m.points,
        annotator_id=annotator.id,
        document_id=m.doc_id,
        color=color,
        answer_id=m.answer_id,
        draw_data=m.draw_data
    )
    db.session.add(ann)
    ann.set_position_info(m.coord)
    db.session.commit()
    return json_response(ann)


def validate_color(color: Optional[str]):
    if color and not is_color_hex_string(color):
        raise RouteException("Color should be a hex string or None, e.g. '#FFFFFF'.")


@dataclass
class AnnotationIdModel:
    id: int


@dataclass
class UpdateAnnotationModel(AnnotationIdModel):
    visible_to: AnnotationVisibility = field(metadata={'by_value': True})
    points: Optional[float] = None
    color: Optional[str] = None
    coord: Optional[AnnotationPosition] = None
    draw_data: Optional[List[dict]] = None


@annotations.route("/update_annotation", methods=['post'])
@use_model(UpdateAnnotationModel)
def update_annotation(m: UpdateAnnotationModel):
    """Updates the information of an annotation.
    """
    verify_logged_in()
    user = get_current_user_object()

    visible_to = m.visible_to
    points = m.points
    color = m.color
    drawing = m.draw_data

    ann = get_annotation_or_abort(m.id)
    d = get_doc_or_abort(ann.document_id)
    if ann.annotator_id != user.id:
        raise AccessDenied("You are not the annotator.")
    if visible_to:
        ann.visible_to = visible_to.value

    # validate_color(color) # TODO: tämä ei salli värien nimiä kuten aqua tms, iPadillä niitä saa kirjoittaa

    ann.color = color

    if has_teacher_access(d):
        ann.points = points
    else:
        if points is None:
            ann.points = points
        else:
            ann.points = None
    if m.coord:
        ann.set_position_info(m.coord)
    if drawing:
        ann.draw_data = json.dumps(drawing)

    db.session.commit()
    return json_response(ann)


def is_color_hex_string(s: str) -> bool:
    """Checks if string is valid HTML color hex string.
    """
    exp = r"#[a-fA-F0-9]{6}"
    check = re.match(exp, s)
    if check is not None:
        return check.group() == s
    return False


@annotations.route("/invalidate_annotation", methods=['post'])
@use_model(AnnotationIdModel)
def invalidate_annotation(m: AnnotationIdModel):
    """Invalidates an annotation by setting its valid_until to current moment.
    """

    verify_logged_in()
    annotation = get_annotation_or_abort(m.id)
    user = get_current_user_object()
    if not annotation.annotator_id == user.id:
        raise AccessDenied("You are not the annotator.")
    annotation.valid_until = get_current_time()
    db.session.commit()
    return ok_response()


@dataclass
class AddAnnotationCommentModel(AnnotationIdModel):
    content: str


def get_annotation_or_abort(ann_id: int) -> Annotation:
    ann = set_annotation_query_opts(Annotation.query).get(ann_id)
    if not ann:
        raise RouteException(f'Annotation with id {ann_id} not found')
    return ann


@annotations.route("/add_annotation_comment", methods=['post'])
@use_model(AddAnnotationCommentModel)
def add_comment_route(m: AddAnnotationCommentModel):
    """Adds a new comment to the annotation.
    """
    verify_logged_in()
    commenter = get_current_user_object()
    a = get_annotation_or_abort(m.id)
    if not m.content:
        raise RouteException('Comment must not be empty')
    a.comments.append(AnnotationComment(content=m.content, commenter_id=commenter.id))
    # TODO: Send email to annotator if commenter is not the annotator.
    db.session.commit()
    return json_response(a)


@annotations.route("/<int:doc_id>/get_annotations", methods=['GET'])
def get_annotations(doc_id: int):
    """Returns all annotations with comments user can see, e.g. has access to them in a document.

    :param doc_id: ID of the document
    """
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)

    results = get_annotations_with_comments_in_document(get_current_user_object(), d)
    return no_cache_json_response(results)
