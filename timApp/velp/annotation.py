"""The module handles the main logic related to annotations. This includes adding, modifiying and deleting annotations
as well as adding comments to the annotations. The module also retrieves the annotations to the document.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""
import re
from dataclasses import dataclass, field
from typing import Optional

from flask import Response

from timApp.answer.answer import Answer
from timApp.answer.routes import verify_answer_access
from timApp.auth.accesshelper import (
    verify_logged_in,
    has_teacher_access,
    get_doc_or_abort,
    verify_view_access,
    AccessDenied,
    verify_teacher_access,
)
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.viewcontext import default_view_ctx
from timApp.peerreview.peerreview_utils import has_review_access
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import (
    json_response,
    ok_response,
    no_cache_json_response,
    to_json_str,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import get_current_time
from timApp.velp.annotation_model import Annotation, AnnotationPosition
from timApp.velp.annotations import (
    AnnotationVisibility,
    get_annotations_with_comments_in_document,
    set_annotation_query_opts,
)
from timApp.velp.velp_models import AnnotationComment
from timApp.velp.velps import get_latest_velp_version

annotations = TypedBlueprint("annotations", __name__, url_prefix="")


@annotations.post("/add_annotation")
def add_annotation(
    velp_id: int,
    doc_id: int,
    coord: AnnotationPosition,
    points: float | None,
    visible_to: AnnotationVisibility = field(metadata={"by_value": True}),
    color: str | None = None,
    answer_id: int | None = None,
    draw_data: list[dict] | None = None,
) -> Response:
    """Adds a new annotation."""
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    validate_color(color)
    annotator = get_current_user_object()
    latest_velp_version = get_latest_velp_version(velp_id)
    if not latest_velp_version:
        raise RouteException(f"Velp with id {velp_id} not found.")
    velp_version_id = latest_velp_version.version_id

    if answer_id:
        try:
            _, ans_doc_id = verify_answer_access(
                answer_id,
                get_current_user_object().id,
                default_view_ctx,
                require_teacher_if_not_own=True,
            )
        except AccessDenied:
            a: Answer = Answer.query.get(answer_id)
            if a.has_many_collaborators:
                raise RouteException(
                    "Reviewing answers with multiple collaborators not supported yet"
                )
            if not has_review_access(d, annotator, a.parsed_task_id, a.users_all[0]):
                raise AccessDenied()
        else:
            if doc_id != ans_doc_id:
                raise RouteException("Answer id does not match the requested document.")

    ann = Annotation(
        velp_version_id=velp_version_id,
        visible_to=visible_to.value,
        points=points,
        annotator_id=annotator.id,
        document_id=doc_id,
        color=color,
        answer_id=answer_id,
        draw_data=draw_data,
    )
    db.session.add(ann)
    ann.set_position_info(coord)
    db.session.commit()
    return json_response(ann, date_conversion=True)


def validate_color(color: str | None) -> None:
    if color and not is_color_hex_string(color):
        raise RouteException("Color should be a hex string or None, e.g. '#FFFFFF'.")


def check_visibility_and_maybe_get_doc(
    user: User, ann: Annotation
) -> tuple[bool, DocInfo | None]:
    d = None
    if user.id == ann.annotator_id:
        return True, d
    if ann.visible_to == AnnotationVisibility.everyone.value:
        return True, d
    d = get_doc_or_abort(ann.document_id)
    if (
        ann.visible_to == AnnotationVisibility.teacher.value
        and user.has_teacher_access(d)
    ):
        return True, d
    if ann.visible_to == AnnotationVisibility.owner.value and user.has_ownership(d):
        return True, d
    return False, d


def check_annotation_edit_access_and_maybe_get_doc(
    user: User, ann: Annotation
) -> tuple[bool, DocInfo | None]:
    vis, d = check_visibility_and_maybe_get_doc(user, ann)
    if not vis:
        return False, d
    if user.id == ann.annotator_id:
        return True, d
    if not d:
        d = get_doc_or_abort(ann.document_id)
    verify_teacher_access(d)
    return True, d


@annotations.post("/update_annotation")
def update_annotation(
    id: int,
    visible_to: AnnotationVisibility = field(metadata={"by_value": True}),
    points: float | None = None,
    color: str | None = None,
    coord: AnnotationPosition | None = None,
    draw_data: list[dict] | None = None,
) -> Response:
    """Updates the information of an annotation."""
    verify_logged_in()
    user = get_current_user_object()

    ann = get_annotation_or_abort(id)
    can_edit, d = check_annotation_edit_access_and_maybe_get_doc(user, ann)
    if not can_edit:
        raise AccessDenied("Sorry, you don't have permission to edit this annotation.")

    if visible_to:
        ann.visible_to = visible_to.value

    # validate_color(color) # TODO: tämä ei salli värien nimiä kuten aqua tms, iPadillä niitä saa kirjoittaa

    ann.color = color

    if not d:
        d = get_doc_or_abort(ann.document_id)
    if has_teacher_access(d):
        ann.points = points
    if coord:
        ann.set_position_info(coord)
    if draw_data:
        ann.draw_data = to_json_str(draw_data)

    db.session.commit()
    return json_response(ann, date_conversion=True)


def is_color_hex_string(s: str) -> bool:
    """Checks if string is valid HTML color hex string."""
    exp = r"#[a-fA-F0-9]{6}"
    check = re.match(exp, s)
    if check is not None:
        return check.group() == s
    return False


@annotations.post("/invalidate_annotation")
def invalidate_annotation(id: int) -> Response:
    """Invalidates an annotation by setting its valid_until to current moment."""

    verify_logged_in()
    annotation = get_annotation_or_abort(id)
    user = get_current_user_object()
    can_edit, _ = check_annotation_edit_access_and_maybe_get_doc(user, annotation)
    if not can_edit:
        raise AccessDenied(
            "Sorry, you don't have permission to delete this annotation."
        )
    annotation.valid_until = get_current_time()
    db.session.commit()
    return ok_response()


def get_annotation_or_abort(ann_id: int) -> Annotation:
    ann = set_annotation_query_opts(Annotation.query).get(ann_id)
    if not ann:
        raise RouteException(f"Annotation with id {ann_id} not found")
    return ann


@annotations.post("/add_annotation_comment")
def add_comment_route(id: int, content: str) -> Response:
    """Adds a new comment to the annotation."""
    verify_logged_in()
    commenter = get_current_user_object()
    a = get_annotation_or_abort(id)
    vis, _ = check_visibility_and_maybe_get_doc(commenter, a)
    if not vis:
        raise AccessDenied(
            "Sorry, you don't have permission to add comments to this annotation."
        )
    if not content:
        raise RouteException("Comment must not be empty")
    a.comments.append(AnnotationComment(content=content, commenter_id=commenter.id))
    # TODO: Send email to annotator if commenter is not the annotator.
    db.session.commit()
    return json_response(a, date_conversion=True)


@annotations.get("/<int:doc_id>/get_annotations")
def get_annotations(doc_id: int, only_own: bool = False) -> Response:
    """Returns all annotations with comments user can see, e.g. has access to them in a document.

    :param doc_id: ID of the document
    :param only_own: If True, only returns annotations of this user.
    """
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)

    results = get_annotations_with_comments_in_document(
        get_current_user_object(), d, only_own
    )
    return no_cache_json_response(results, date_conversion=True)
