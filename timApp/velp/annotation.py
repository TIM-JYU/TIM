"""The module handles the main logic related to annotations. This includes adding, modifiying and deleting annotations
as well as adding comments to the annotations. The module also retrieves the annotations to the document.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""
import re
from dataclasses import field

from flask import Response
from sqlalchemy import select

from timApp.answer.answer import Answer
from timApp.auth.accesshelper import (
    verify_logged_in,
    has_teacher_access,
    get_doc_or_abort,
    verify_view_access,
    AccessDenied,
    verify_teacher_access,
    has_seeanswers_access,
    verify_answer_access,
)
from timApp.auth.get_user_rights_for_item import get_user_rights_for_item
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.document.viewcontext import default_view_ctx
from timApp.peerreview.util.peerreview_utils import (
    has_review_access,
    get_reviews_targeting_user,
    get_all_reviews,
    get_reviews_related_to_user,
)
from timApp.timdb.sqa import db
from timApp.user.user import User, has_no_higher_right
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
    style: int | None = None,
) -> Response:
    """Adds a new annotation."""
    d = get_doc_or_abort(doc_id)
    if not has_teacher_access(d) and d.document.get_settings().exam_mode():
        raise AccessDenied("You cannot add annotations to this document.")
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
            a: Answer = db.session.get(Answer, answer_id)
            if a.has_many_collaborators:
                raise RouteException(
                    "Reviewing answers with multiple collaborators not supported yet"
                )
            if not has_review_access(d, annotator, None, a.users_all[0]):
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
        style=style,
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
    style: int | None = None,
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
    if style:
        ann.style = style

    db.session.commit()
    if should_anonymize_annotations(d, user):
        anonymize_annotations([ann], user.id)
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
    user = get_current_user_object()
    annotation = get_annotation_or_abort(id)
    can_edit, _ = check_annotation_edit_access_and_maybe_get_doc(user, annotation)
    if not can_edit:
        raise AccessDenied(
            "Sorry, you don't have permission to delete this annotation."
        )
    annotation.valid_until = get_current_time()
    db.session.commit()
    return ok_response()


def get_annotation_or_abort(ann_id: int) -> Annotation:
    # Possibly bug: We need to create a new query object, otherwise raiseload() seems to pollute User's relations
    ann = (
        db.session.execute(
            set_annotation_query_opts(select(Annotation).filter_by(id=ann_id)).limit(1)
        )
        .scalars()
        .first()
    )
    if not ann:
        raise RouteException(f"Annotation with id {ann_id} not found")
    return ann


@annotations.post("/add_annotation_comment")
def add_comment_route(id: int, content: str) -> Response:
    """Adds a new comment to the annotation."""
    verify_logged_in()
    commenter = get_current_user_object()
    a = get_annotation_or_abort(id)
    vis, d = check_visibility_and_maybe_get_doc(commenter, a)
    if not vis:
        raise AccessDenied(
            "Sorry, you don't have permission to add comments to this annotation."
        )
    if not content:
        raise RouteException("Comment must not be empty")
    if not d:
        d = get_doc_or_abort(a.document_id)
    a.comments.append(AnnotationComment(content=content, commenter_id=commenter.id))
    # TODO: Send email to annotator if commenter is not the annotator.
    db.session.commit()
    if should_anonymize_annotations(d, commenter):
        anonymize_annotations([a], commenter.id)
    return json_response(a, date_conversion=True)


def should_anonymize_annotations(d: DocInfo, u: User) -> bool:
    """
    Determines whether annotation author and comment authors should be hidden from a user
    """
    rights = get_user_rights_for_item(d, u)
    return has_no_higher_right(d.document.get_settings().anonymize_reviewers(), rights)


def anonymize_annotations(anns: list[Annotation], current_user_id: int) -> None:
    """
    Fully anonymizes annotation authors and comment authors unless they were created by same user
    """
    for ann in anns:
        if ann.annotator.id != current_user_id:
            ann.annotator.anonymize = True
        for c in ann.comments:
            if c.commenter.id != current_user_id:
                c.commenter.anonymize = True


@annotations.get("/<int:doc_id>/get_annotations")
def get_annotations(doc_id: int, only_own: bool = False) -> Response:
    """Returns all annotations with comments user can see, e.g. has access to them in a document.
    Also returns all PeerReview rows user can see

    :param doc_id: ID of the document
    :param only_own: If True, only returns annotations made by this user and peer_reviews where this user is the reviewer.
    """
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)

    current_user = get_current_user_object()
    results = get_annotations_with_comments_in_document(current_user, d, only_own)
    orig_doc_info = d if d.is_original_translation else d.src_doc
    if not only_own:
        # TODO: check use cases (only_own == not in view tab)
        #  teachermode should be able to browse all prs when changing user in ab
        #  review tab needs probably only reviews where reviewer is current user
        if has_teacher_access(d):
            peer_reviews = get_all_reviews(orig_doc_info)
        else:
            peer_reviews = get_reviews_related_to_user(orig_doc_info, current_user)
    else:
        peer_reviews = get_reviews_targeting_user(orig_doc_info, current_user)

    # TODO: Fully anonymize peer_reviews if doc setting requires it
    if should_anonymize_annotations(d, current_user):
        curruser_id = current_user.id
        anonymize_annotations(results, curruser_id)
        # TODO: We can't anonymize pr rows yet:
        #   - user id is needed to find correct target when saving the review,
        #   and anonymized users get id -1.
        # for p in peer_reviews:
        #     if p.reviewer_id != current_user.id:
        #         p.reviewer.anonymize = True
        #     if p.reviewable_id != current_user.id:
        #         p.reviewable.anonymize = True
    elif not has_seeanswers_access(d):
        # TODO: these checks should be changed to something else
        #  - in future peerreview pairing may be changeable, but anonymization info should persist
        #  - instead of comparing peer_reviews every time annotation could directly contain info about anonymization
        revset = {r.reviewer_id for r in peer_reviews}
        for ann in results:
            if ann.annotator.id != current_user.id and ann.annotator_id in revset:
                ann.annotator.hide_name = True
        for p in peer_reviews:
            if p.reviewer_id != current_user.id:
                p.reviewer.hide_name = True
            if p.reviewable_id != current_user:
                p.reviewable.hide_name = True

    return no_cache_json_response(
        {"annotations": results, "peer_reviews": peer_reviews}, date_conversion=True
    )
