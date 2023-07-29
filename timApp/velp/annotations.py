"""The module contains the database functions related to annotations. This includes adding, modifiying and deleting
annotations as well as adding comments to the annotations. The module also retrieves the annotations from the database.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

from enum import Enum, unique

from sqlalchemy import func, true, select
from sqlalchemy.orm import selectinload, joinedload
from sqlalchemy.sql import Select

from timApp.answer.answer import Answer
from timApp.document.docinfo import DocInfo
from timApp.peerreview.peerreview import PeerReview
from timApp.peerreview.util.peerreview_utils import (
    get_reviews_where_user_is_reviewer_query,
    is_peerreview_enabled,
)
from timApp.timdb.sqa import run_sql
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
    own_review_filter = true()
    if not user.has_seeanswers_access(d) or only_own:
        answer_filter = (User.id == user.id) | (User.id == None)
        if is_peerreview_enabled(d):
            answer_filter |= User.id.in_(
                get_reviews_where_user_is_reviewer_query(d, user).with_only_columns(
                    PeerReview.reviewable_id
                )
            )
            own_review_filter = (User.id == user.id) | (
                Annotation.annotator_id == user.id
            )

    q = (
        set_annotation_query_opts(
            select(Annotation)
            .filter_by(document_id=d.id)
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
            .filter(own_review_filter)
            .order_by(
                Annotation.depth_start.desc(),
                Annotation.node_start.desc(),
                Annotation.offset_start.desc(),
            )
        )
        .options(joinedload(Annotation.answer).selectinload(Answer.users_all))
        .with_only_columns(Annotation)
    )
    anns = run_sql(q).scalars().all()
    return anns


def set_annotation_query_opts(q: Select) -> Select:
    return (
        q.options(joinedload(Annotation.velp_content).load_only(VelpContent.content))
        .options(
            selectinload(Annotation.comments)
            .joinedload(AnnotationComment.commenter)
            .raiseload(User.groups)
        )
        .options(selectinload(Annotation.annotator).raiseload(User.groups))
        .options(
            joinedload(Annotation.answer)
            .selectinload(Answer.users_all)
            .raiseload(User.groups)
        )
        .options(
            joinedload(Annotation.velp_version)
            .load_only(VelpVersion.id, VelpVersion.velp_id)
            .joinedload(VelpVersion.velp)
            .load_only(Velp.color)
        )
    )
