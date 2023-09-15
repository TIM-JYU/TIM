"""
Short description of Python module
"""

__authors__ = ["Mika Lehtinen", "Simo Lehtinen", "Alexander Södergård"]
__license__ = "MIT"
__date__ = "29.5.2022"

from sqlalchemy import select
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import selectinload
from sqlalchemy.sql import Select

from timApp.document.docinfo import DocInfo
from timApp.peerreview.peerreview import PeerReview
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from timApp.util.flask.requesthelper import RouteException
from timApp.util.utils import get_current_time


def get_reviews_where_user_is_reviewer(d: DocInfo, user: User) -> list[PeerReview]:
    """Return all peer_review rows where block_is is d.id and the person making the review is the given user"""
    stmt = get_reviews_where_user_is_reviewer_query(d, user).options(
        selectinload(PeerReview.reviewable)
    )
    return run_sql(stmt).scalars().all()  # type: ignore


def get_reviews_where_user_is_reviewer_query(d: DocInfo, user: User) -> Select:
    return select(PeerReview).filter_by(block_id=d.id, reviewer_id=user.id)


def get_all_reviews(doc: DocInfo) -> list[PeerReview]:
    return run_sql(select(PeerReview).filter_by(block_id=doc.id)).scalars().all()  # type: ignore


def get_reviews_targeting_user(d: DocInfo, user: User) -> list[PeerReview]:
    """Return all peer_review rows where block_id is d.id and the user is the review target"""
    stmt = get_reviews_targeting_user_query(d, user).options(
        selectinload(PeerReview.reviewable)
    )
    return run_sql(stmt).scalars().all()  # type: ignore


def get_reviews_targeting_user_query(d: DocInfo, user: User) -> Select:
    return select(PeerReview).filter_by(block_id=d.id, reviewable_id=user.id)


def get_reviews_related_to_user(d: DocInfo, user: User) -> list[PeerReview]:
    stmt = (
        select(PeerReview)
        .filter_by(block_id=d.id)
        .filter(
            (PeerReview.reviewable_id == user.id) | (PeerReview.reviewer_id == user.id)
        )
    )
    return run_sql(stmt).scalars().all()  # type: ignore


def has_review_access(
    doc: DocInfo,
    reviewer_user: User,
    task_id: TaskId | None,
    reviewable_user: User | None,
) -> bool:
    if not is_peerreview_enabled(doc):
        return False
    stmt = select(PeerReview).filter_by(
        block_id=doc.id,
        reviewer_id=reviewer_user.id,
    )
    if task_id is not None:
        stmt = stmt.filter_by(task_name=task_id.task_name)
    if reviewable_user is not None:
        stmt = stmt.filter_by(reviewable_id=reviewable_user.id)
    return bool(run_sql(stmt.limit(1)).scalars().first())


def check_review_grouping(doc: DocInfo, tasks: list[TaskId]) -> bool:
    # TODO: new tasks may be added to area after pr starts
    #   => if any tasks in area (list of tasks) has pr rows, then copy the pairings to tasks in list missing pr rows
    stmt = (
        select(PeerReview)
        .filter_by(block_id=doc.id)
        .filter(PeerReview.task_name.in_([t.task_name for t in tasks]))
    )
    return bool(run_sql(stmt.limit(1)).scalars().first())


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
    return (
        run_sql(
            select(PeerReview).filter_by(
                block_id=doc.id,
            )
        )
        .scalars()
        .all()  # type: ignore
    )


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
        updated_user = (
            run_sql(
                select(PeerReview)
                .filter_by(
                    block_id=doc.id,
                    reviewer_id=old_reviewers[i],
                    reviewable_id=reviewable,
                    task_name=task,
                )
                .limit(1)
            )
            .scalars()
            .first()
        )
        if updated_user:
            updated_user.reviewer_id = new_reviewers[i]
    try:
        db.session.flush()
    except IntegrityError:
        db.session.rollback()
        raise RouteException("Error: Same reviewer more than once in one task")
    # Note: Don't commit here because the function is used in other functions that update the DB
