"""
Short description of Python module
"""

__authors__ = ["Mika Lehtinen", "Simo Lehtinen", "Alexander Södergård"]
__license__ = "MIT"
__date__ = "29.5.2022"

from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import joinedload, Query

from timApp.document.docinfo import DocInfo
from timApp.peerreview.peerreview import PeerReview
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException
from timApp.util.utils import get_current_time


def get_reviews_where_user_is_reviewer(d: DocInfo, user: User) -> list[PeerReview]:
    """Return all peer_review rows where block_is is d.id and the person making the review is the given user"""
    q = get_reviews_where_user_is_reviewer_query(d, user).options(
        joinedload(PeerReview.reviewable)
    )
    return q.all()


def get_reviews_where_user_is_reviewer_query(d: DocInfo, user: User) -> Query:
    return PeerReview.query.filter_by(block_id=d.id, reviewer_id=user.id)


def get_all_reviews(doc: DocInfo) -> list[PeerReview]:
    return PeerReview.query.filter_by(block_id=doc.id).all()


def get_reviews_targeting_user(d: DocInfo, user: User) -> list[PeerReview]:
    """Return all peer_review rows where block_id is d.id and the user is the review target"""
    q = get_reviews_targeting_user_query(d, user).options(
        joinedload(PeerReview.reviewable)
    )
    return q.all()


def get_reviews_targeting_user_query(d: DocInfo, user: User) -> Query:
    return PeerReview.query.filter_by(block_id=d.id, reviewable_id=user.id)


def get_reviews_related_to_user(d: DocInfo, user: User) -> list[PeerReview]:
    q = PeerReview.query.filter_by(block_id=d.id).filter(
        (PeerReview.reviewable_id == user.id) | (PeerReview.reviewer_id == user.id)
    )
    return q.all()


def has_review_access(
    doc: DocInfo,
    reviewer_user: User,
    task_id: TaskId | None,
    reviewable_user: User | None,
) -> bool:
    if not is_peerreview_enabled(doc):
        return False
    q = PeerReview.query.filter_by(
        block_id=doc.id,
        reviewer_id=reviewer_user.id,
    )
    if task_id is not None:
        q = q.filter_by(task_name=task_id.task_name)
    if reviewable_user is not None:
        q = q.filter_by(reviewable_id=reviewable_user.id)
    return bool(q.first())


def check_review_grouping(doc: DocInfo, tasks: list[TaskId]) -> bool:
    # TODO: new tasks may be added to area after pr starts
    #   => if any tasks in area (list of tasks) has pr rows, then copy the pairings to tasks in list missing pr rows
    q = PeerReview.query.filter_by(block_id=doc.id).filter(
        PeerReview.task_name.in_([t.task_name for t in tasks])
    )
    return bool(q.first())


def is_peerreview_enabled(doc: DocInfo) -> bool:
    settings = doc.document.get_settings()
    peer_review_start = settings.peer_review_start()
    peer_review_stop = settings.peer_review_stop()

    if not peer_review_start or not peer_review_stop:
        return doc.document.get_settings().peer_review()

    return peer_review_start <= get_current_time() < peer_review_stop


def get_reviews_for_document(doc: DocInfo) -> list[PeerReview]:
    """Get peer-reviewers of current document from the database.
    :param doc: Document containing reviewable answers.
    """
    return PeerReview.query.filter_by(
        block_id=doc.id,
    ).all()


def change_peerreviewers_for_user(
    doc: DocInfo,
    task: str,
    reviewable: int,
    old_reviewers: list[int],
    new_reviewers: list[int],
) -> None:
    """Change reviewers in one task.
    :param doc: Document containing reviewable answers.
    :param task: task name.
    :param reviewable: User ID for the review target user.
    :param old_reviewers: List of old reviewers IDs
    :param new_reviewers: List of new reviewers IDs
    """
    # TODO: Clean up; don't do one query per loop: instead fetch all users at once!
    for i in range(0, len(new_reviewers)):
        updated_user = PeerReview.query.filter_by(
            block_id=doc.id,
            reviewer_id=old_reviewers[i],
            reviewable_id=reviewable,
            task_name=task,
        ).first()
        updated_user.reviewer_id = new_reviewers[i]
    try:
        db.session.flush()
    except IntegrityError:
        db.session.rollback()
        raise RouteException("Error: Same reviewer more than once in one task")
    # Note: Don't commit here because the function is used in other functions that update the DB
