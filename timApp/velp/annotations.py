"""The module contains the database functions related to annotations. This includes adding, modifiying and deleting
annotations as well as adding comments to the annotations. The module also retrieves the annotations from the database.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

from enum import Enum, unique
from typing import List, Optional

from sqlalchemy import func, true
from sqlalchemy.orm import joinedload

from timApp.answer.answer import Answer
from timApp.answer.answer_models import UserAnswer
from timApp.document.docinfo import DocInfo
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.velp.velp_models import VelpContent, VelpVersion, Velp, AnnotationComment
from timApp.velp.annotation_model import Annotation


@unique
class AnnotationVisibility(Enum):
    """Enum for storing the visibility.
    """
    myself = 1
    owner = 2
    teacher = 3
    everyone = 4


def create_annotation(
        velp_version_id: int,
        visible_to: AnnotationVisibility,
        points: Optional[float],
        annotator_id: int,
        document_id: int,
        valid_until: Optional[str] = None,
        icon_id: Optional[int] = None,
        color: Optional[str] = "",
        answer_id: Optional[int] = None,
) -> Annotation:
    """Create a new annotation.

    :param velp_version_id: Version of the velp that the annotation uses.
    :param visible_to: visibility of the annotation.
    :param points: Points given, overrides velp's default and can be null.
    :param annotator_id: ID of user who left the annotation.
    :param document_id: ID of document in which annotation is located in.
    :param valid_until: Datetime until which annotation is valid for, 'none' for forever.
    :param icon_id: ID of icon associated with annotation, can be 'none'.
    :param color: Color as hex string.
    :param answer_id: ID of answer if annotation is located within one.
    :return: the new annotation.

    """

    a = Annotation(
        velp_version_id=velp_version_id,
        visible_to=visible_to.value,
        points=points,
        valid_until=valid_until,
        icon_id=icon_id,
        color=color,
        annotator_id=annotator_id,
        document_id=document_id,
        answer_id=answer_id,
    )
    db.session.add(a)
    return a


def get_annotations_with_comments_in_document(user: User, d: DocInfo) -> List[Annotation]:
    """Gets all annotations with comments the user can see / has access to.
    """
    language_id = 'FI'
    vis_filter = Annotation.visible_to == AnnotationVisibility.everyone.value
    if user.has_teacher_access(d):
        vis_filter = vis_filter | (Annotation.visible_to == AnnotationVisibility.teacher.value)
    if user.has_ownership(d):
        vis_filter = vis_filter | (Annotation.visible_to == AnnotationVisibility.owner.value)
    answer_filter = true()
    if not user.has_seeanswers_access(d):
        answer_filter = (UserAnswer.user_id == user.id) | (UserAnswer.user_id == None)
    anns = (set_annotation_query_opts(Annotation.query
        .filter_by(document_id=d.id)
        .filter((Annotation.valid_until == None) | (Annotation.valid_until >= func.current_timestamp()))
        .filter((Annotation.annotator_id == user.id) | vis_filter)
        .join(VelpVersion)
        .join(Velp)
        .join(VelpContent)
        .filter(VelpContent.language_id == language_id)
        .outerjoin(UserAnswer, UserAnswer.answer_id == Annotation.answer_id)
        .filter(answer_filter)
        .order_by(
        Annotation.depth_start.desc(),
        Annotation.node_start.desc(),
        Annotation.offset_start.desc(),
    ))
            .with_entities(Annotation)
            .all())
    return anns


def set_annotation_query_opts(q):
    return (q.options(joinedload(Annotation.velp_content))
            .options(joinedload(Annotation.comments).joinedload(AnnotationComment.commenter))
            .options(joinedload(Annotation.annotator))
            .options(joinedload(Annotation.answer).joinedload(Answer.users_all))
            .options(joinedload(Annotation.velp_version).joinedload(VelpVersion.velp)))
