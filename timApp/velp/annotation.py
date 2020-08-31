"""The module handles the main logic related to annotations. This includes adding, modifiying and deleting annotations
as well as adding comments to the annotations. The module also retrieves the annotations to the document.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""
import json
import re
from dataclasses import dataclass, field
from typing import Optional, List, Tuple

from flask import Blueprint, Response

from timApp.answer.routes import verify_answer_access
from timApp.auth.accesshelper import verify_logged_in, has_teacher_access, \
    get_doc_or_abort, verify_view_access, AccessDenied
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException, use_model
from timApp.util.flask.responsehelper import json_response, ok_response, no_cache_json_response, to_json_str
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
def add_annotation(m: AddAnnotationModel) -> Response:
    """Adds a new annotation.
    """
    d = get_doc_or_abort(m.doc_id)
    verify_view_access(d)
    color = m.color
    validate_color(color)
    annotator = get_current_user_object()
    latest_velp_version = get_latest_velp_version(m.velp_id)
    if not latest_velp_version:
        raise RouteException("f'Velp with id {m.velp_id} not found'")
    velp_version_id = latest_velp_version.version_id

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


def validate_color(color: Optional[str]) -> None:
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


def check_visibility_and_maybe_get_doc(user: User, ann: Annotation) -> Tuple[bool, Optional[DocInfo]]:
    d = None
    if user.id == ann.annotator_id:
        return True, d
    if ann.visible_to == AnnotationVisibility.everyone.value:
        return True, d
    d = get_doc_or_abort(ann.document_id)
    if ann.visible_to == AnnotationVisibility.teacher.value and user.has_teacher_access(d):
        return True, d
    if ann.visible_to == AnnotationVisibility.owner.value and user.has_ownership(d):
        return True, d
    return False, d


def check_annotation_edit_access_and_maybe_get_doc(user: User, ann: Annotation) -> Tuple[bool, Optional[DocInfo]]:
    vis, d = check_visibility_and_maybe_get_doc(user, ann)
    if user.id == ann.annotator_id:
        return True, d
    if not d:
        d = get_doc_or_abort(ann.id)
    return user.has_teacher_access(d) is not None, d


@annotations.route("/update_annotation", methods=['post'])
@use_model(UpdateAnnotationModel)
def update_annotation(m: UpdateAnnotationModel) -> Response:
    """Updates the information of an annotation.
    """
    verify_logged_in()
    user = get_current_user_object()

    visible_to = m.visible_to
    points = m.points
    color = m.color
    drawing = m.draw_data

    ann = get_annotation_or_abort(m.id)
    can_edit, d = check_annotation_edit_access_and_maybe_get_doc(user, ann)
    if not can_edit:
        raise AccessDenied("Sorry, you don't have permission to edit this annotation")

    if not d:
        d = get_doc_or_abort(m.id)

    if visible_to:
        ann.visible_to = visible_to.value

    # validate_color(color) # TODO: tämä ei salli värien nimiä kuten aqua tms, iPadillä niitä saa kirjoittaa

    ann.color = color

    if has_teacher_access(d):
        ann.points = points
    if m.coord:
        ann.set_position_info(m.coord)
    if drawing:
        ann.draw_data = to_json_str(drawing)

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
def invalidate_annotation(m: AnnotationIdModel) -> Response:
    """Invalidates an annotation by setting its valid_until to current moment.
    """

    verify_logged_in()
    annotation = get_annotation_or_abort(m.id)
    user = get_current_user_object()
    can_edit, _ = check_annotation_edit_access_and_maybe_get_doc(user, annotation)
    if not can_edit:
        raise AccessDenied("Sorry, you don't have permission to delete this annotation")
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
def add_comment_route(m: AddAnnotationCommentModel) -> Response:
    """Adds a new comment to the annotation.
    """
    verify_logged_in()
    commenter = get_current_user_object()
    a = get_annotation_or_abort(m.id)
    vis, _ = check_visibility_and_maybe_get_doc(commenter, a)
    if not vis:
        raise AccessDenied("Sorry, you don't have permission to add comments to this annotation")
    if not m.content:
        raise RouteException('Comment must not be empty')
    a.comments.append(AnnotationComment(content=m.content, commenter_id=commenter.id))
    # TODO: Send email to annotator if commenter is not the annotator.
    db.session.commit()
    return json_response(a)


@annotations.route("/<int:doc_id>/get_annotations", methods=['GET'])
def get_annotations(doc_id: int) -> Response:
    """Returns all annotations with comments user can see, e.g. has access to them in a document.

    :param doc_id: ID of the document
    """
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)

    results = get_annotations_with_comments_in_document(get_current_user_object(), d)
    return no_cache_json_response(results)
