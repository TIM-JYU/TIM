"""
Short description of Python module
"""

__authors__ = ["Mika Lehtinen", "Simo Lehtinen", "Alexander Södergård"]
__license__ = "MIT"
__date__ = "29.5.2022"

from collections import defaultdict
from datetime import datetime
from typing import DefaultDict

from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import joinedload, Query

from timApp.answer.answer import Answer
from timApp.answer.answers import get_points_by_rule, get_latest_valid_answers_query
from timApp.document.docinfo import DocInfo
from timApp.peerreview.peerreview import PeerReview
from timApp.plugin.plugin import Plugin
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
import dateutil.parser
import pytz

from timApp.util.flask.requesthelper import RouteException


class PeerReviewException(Exception):
    pass


# Generate reviews groups for list
def generate_review_groups(doc: DocInfo, tasks: list[Plugin]) -> None:
    task_ids = []

    for task in tasks:
        if task.task_id:
            task_ids.append(task.task_id)

    points = get_points_by_rule(None, task_ids, None)

    users = []
    for user in points:
        users.append(user["user"])

    settings = doc.document.get_settings()
    review_count = settings.peer_review_count()

    # TODO: [Kuvio] get timestamps from doc settings
    start_time_reviews = datetime.now()
    end_time_reviews = datetime.now()

    # Dictionary containing review pairs,
    # has reviewer user ID as key and value is list containing reviewable user IDs
    pairing: DefaultDict[int, list[int]] = defaultdict(list)

    if len(users) < 2:
        raise PeerReviewException(
            f"Not enough users to form pairs ({len(users)} but at least 2 users needed)"
        )

    if review_count <= 0:
        raise PeerReviewException(
            f"peer_review_count setting is too low ({review_count})"
        )

    # Decreases review count if it exceeds available user count
    if review_count >= len(users):
        review_count = len(users) - 1

    for idx, user in enumerate(users):
        pairings_left = review_count + 1
        start = idx + 1
        end = idx + pairings_left
        for x in range(start, end):
            if x > len(users) - 1:
                end = end - x
                start = 0
                for i in range(start, end):
                    pairing[users[idx].id].append(users[i].id)
                    if i == end:
                        break
            else:
                pairing[users[idx].id].append(users[x].id)

    for t in task_ids:
        answers: list[Answer] = get_latest_valid_answers_query(t, users).all()
        excluded_users: list[User] = []
        filtered_answers = []
        if not answers:
            # Skip tasks that has no answers
            continue
        for answer in answers:
            if len(answer.users_all) > 1:
                # TODO: Implement handling for multiple users
                continue
            else:
                if answer.users_all[0] not in excluded_users:
                    filtered_answers.append(answer)
                    excluded_users.append(answer.users_all[0])

        for reviewer, reviewables in pairing.items():
            for a in filtered_answers:
                if a.users_all[0].id in reviewables:

                    # save_review(
                    #    doc, reviewer, start_time_reviews, end_time_reviews, a, t, False
                    # )
                    save_review(
                        a, t, doc, reviewer, start_time_reviews, end_time_reviews, False
                    )

    db.session.commit()


# def save_review(
#        doc: DocInfo,
#        reviewer_id: int,
#        reviewable_id: int,
#        start_time: datetime,
#        end_time: datetime,
#        answer: Answer | None = None,
#        task_id: TaskId | None = None,
#        reviewed: bool = False,
# ) -> PeerReview:
def save_review(
    answer: Answer,
    task_id: TaskId,
    doc: DocInfo,
    reviewer_id: int,
    start_time: datetime,
    end_time: datetime,
    reviewed: bool = False,
) -> PeerReview:
    """Saves a review to the database.

    :param doc: Document containing reviewable answers.
    :param reviewer_id: User ID for the reviewer user.
    :param reviewable_id: User ID for the review target user.
    :param start_time: Timestamp for starting the review period.
    :param end_time: Timestamp for ending the review period.
    :param answer: Answer object for answer id.
    :param task_id: TaskId object to provide task name.
    :param reviewed: Boolean indicating if review has been done.
    """
    review = PeerReview(
        answer_id=answer.id if answer else None,
        task_name=task_id.task_name if task_id else None,
        block_id=doc.id,
        reviewer_id=reviewer_id,
        reviewable_id=answer.users_all[0].id,
        start_time=start_time,
        end_time=end_time,
        reviewed=reviewed,
    )

    db.session.add(review)
    return review


def get_reviews_for_user(d: DocInfo, user: User) -> list[PeerReview]:
    q = get_reviews_for_user_query(d, user).options(joinedload(PeerReview.reviewable))
    return q.all()


def get_reviews_for_user_query(d: DocInfo, user: User) -> Query:
    return PeerReview.query.filter_by(block_id=d.id, reviewer_id=user.id)


def get_all_reviews(doc: DocInfo) -> list[PeerReview]:
    return PeerReview.query.filter_by(block_id=doc.id).all()


def get_all_reviews(doc: DocInfo) -> list[PeerReview]:
    return PeerReview.query.filter_by(block_id=doc.id).all()


def get_reviews_to_user(d: DocInfo, user: User) -> list[PeerReview]:
    q = get_reviews_to_user_query(d, user).options(joinedload(PeerReview.reviewable))
    return q.all()


def get_reviews_to_user_query(d: DocInfo, user: User) -> Query:
    return PeerReview.query.filter_by(block_id=d.id, reviewable_id=user.id)


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


def check_review_grouping(doc: DocInfo) -> bool:
    q = PeerReview.query.filter_by(block_id=doc.id)
    return bool(q.first())


def is_peerreview_enabled(doc: DocInfo) -> bool:
    return doc.document.get_settings().peer_review()


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
) -> bool:
    """Change reviewers in one task.
    :param doc: Document containing reviewable answers.
    :param task: task name.
    :param reviewable: User ID for the review target user.
    :param old_reviewers: List of old reviewers IDs
    :param new_reviewers: List of new reviewers IDs
    """
    for i in range(0, len(new_reviewers)):
        try:
            updated_user = PeerReview.query.filter_by(
                block_id=doc.id,
                reviewer_id=old_reviewers[i],
                reviewable_id=reviewable,
                task_name=task,
            ).first()
            updated_user.reviewer_id = new_reviewers[i]
            db.session.commit()
        except IntegrityError:
            raise RouteException("Same reviewer more than once in one task")

    return True
