"""The module contains the database functions related to annotations. This includes adding, modifiying and deleting
annotations as well as adding comments to the annotations. The module also retrieves the annotations from the database.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

from enum import Enum, unique

from sqlalchemy import func, true
from sqlalchemy.orm import joinedload, contains_eager, Query

from timApp.answer.answer import Answer
from timApp.document.docinfo import DocInfo
from timApp.peerreview.peerreview import PeerReview
from timApp.peerreview.peerreview_utils import (
    get_reviews_for_user_query,
    is_peerreview_enabled,
)
from timApp.user.user import User
from timApp.velp.annotation_model import Annotation
from timApp.velp.velp_models import VelpContent, VelpVersion, Velp, AnnotationComment


@unique
class AnnotationVisibility(Enum):
    """Enum for storing the visibility."""

    myself = 1
    owner = 2
    teacher = 3
    everyone = 4


def get_annotations_with_comments_in_document(
    user: User, d: DocInfo, only_own: bool = False
) -> list[Annotation]:
    """
    Gets all annotations with comments the user can see / has access to.

    :param user: The user who gets the answers.
    :param d: The document the answers are searched from.
    :param only_own: If True, only annotations for this user are searched even if the user has access to more answers.
    :return: List of annotations.
    """
    language_id = "FI"
    vis_filter = Annotation.visible_to == AnnotationVisibility.everyone.value
    if user.has_teacher_access(d):
        vis_filter = vis_filter | (
            Annotation.visible_to == AnnotationVisibility.teacher.value
        )
    if user.has_ownership(d):
        vis_filter = vis_filter | (
            Annotation.visible_to == AnnotationVisibility.owner.value
        )
    answer_filter = true()
    if not user.has_seeanswers_access(d) or only_own:
        answer_filter = (User.id == user.id) | (User.id == None)
        if is_peerreview_enabled(d):
            answer_filter |= User.id.in_(
                get_reviews_for_user_query(d, user)
                .with_entities(PeerReview.reviewable_id)
                .subquery()
            )
    anns = (
        set_annotation_query_opts(
            Annotation.query.filter_by(document_id=d.id)
            .filter(
                (Annotation.valid_until == None)
                | (Annotation.valid_until >= func.current_timestamp())
            )
            .filter((Annotation.annotator_id == user.id) | vis_filter)
            .join(VelpVersion)
            .join(Velp)
            .join(VelpContent)
            .filter(VelpContent.language_id == language_id)
            .outerjoin(Answer)
            .outerjoin(User, Answer.users_all)
            .filter(answer_filter)
            .order_by(
                Annotation.depth_start.desc(),
                Annotation.node_start.desc(),
                Annotation.offset_start.desc(),
            )
        )
        .options(contains_eager(Annotation.velp_content))
        .options(contains_eager(Annotation.answer).contains_eager(Answer.users_all))
        .options(
            contains_eager(Annotation.velp_version).contains_eager(VelpVersion.velp)
        )
        .with_entities(Annotation)
        .all()
    )
    return anns


def set_annotation_query_opts(q: Query) -> Query:
    return (
        q.options(
            joinedload(Annotation.velp_content, innerjoin=True).load_only(
                VelpContent.content
            )
        )
        .options(
            joinedload(Annotation.comments)
            .joinedload(AnnotationComment.commenter, innerjoin=True)
            .raiseload(User.groups)
        )
        .options(
            joinedload(Annotation.annotator, innerjoin=True).raiseload(User.groups)
        )
        .options(
            joinedload(Annotation.answer)
            .joinedload(Answer.users_all)
            .raiseload(User.groups)
        )
        .options(
            joinedload(Annotation.velp_version, innerjoin=True)
            .load_only(VelpVersion.id, VelpVersion.velp_id)
            .joinedload(VelpVersion.velp)
            .load_only(Velp.color)
        )
    )
