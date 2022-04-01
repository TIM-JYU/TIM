""""""
from collections import defaultdict
from datetime import datetime
from random import shuffle
from typing import DefaultDict

from sqlalchemy.orm import joinedload, Query

from timApp.answer.answer import Answer
from timApp.answer.answers import get_points_by_rule
from timApp.document.docinfo import DocInfo
from timApp.peerreview.peerreview import PeerReview
from timApp.plugin.plugin import Plugin
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


class PeerReviewException(Exception):
    pass


# Generate reviews groups for list
def generate_review_groups(
    doc: DocInfo, tasks: list[Plugin], usergroup: str | None = None
) -> None:
    task_ids = []

    for task in tasks:
        if task.task_id:
            task_ids.append(task.task_id)

    points = get_points_by_rule(None, task_ids, None)

    users = []

    if usergroup is not None:
        ug = UserGroup.get_by_name(usergroup)
        if not ug:
            raise PeerReviewException(f"User group {usergroup} not found")
        userfilter = set(user.id for user in ug.users)
        for user in points:
            if user["user"].id in userfilter:
                users.append(user["user"])
    else:
        for user in points:
            users.append(user["user"])

    shuffle(users)
    settings = doc.document.get_settings()
    review_count = settings.peer_review_count()

    # TODO: [Kuvio] get timestamps from doc settings
    start_time_reviews = settings.peer_review_start()
    end_time_reviews = settings.peer_review_stop()

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

    for reviewer, reviewables in pairing.items():
        for target in reviewables:
            save_review(doc, reviewer, target, start_time_reviews, end_time_reviews)

    # TODO: Before any actual use peer_reviews were strict on target task_id and answer_id,
    #  but currently peer_review is based only on target user and document.
    # for t in task_ids:
    #     answers: list[Answer] = get_latest_valid_answers_query(t, users).all()
    #     excluded_users: list[User] = []
    #     filtered_answers = []
    #     if not answers:
    #         # Skip tasks that has no answers
    #         continue
    #     for answer in answers:
    #         if len(answer.users_all) > 1:
    #             # TODO: Implement handling for multiple users
    #             continue
    #         else:
    #             if answer.users_all[0] not in excluded_users:
    #                 filtered_answers.append(answer)
    #                 excluded_users.append(answer.users_all[0])
    #
    #     for reviewer, reviewables in pairing.items():
    #         for a in filtered_answers:
    #             if a.users_all[0].id in reviewables:
    #                 save_review(
    #                     doc, reviewer, start_time_reviews, end_time_reviews, a, t, False
    #                 )

    db.session.commit()


def save_review(
    doc: DocInfo,
    reviewer_id: int,
    reviewable_id: int,
    start_time: datetime,
    end_time: datetime,
    answer: Answer | None = None,
    task_id: TaskId | None = None,
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
        reviewable_id=reviewable_id,
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
    settings = doc.document.get_settings()
    start = settings.peer_review_start()
    stop = settings.peer_review_stop()
    current_time = datetime.now()

    if not start or not stop:
        return False
    if start <= current_time < stop:
        return True
    else:
        return False


def get_reviews_for_document(doc: DocInfo) -> list[PeerReview]:
    return PeerReview.query.filter_by(
        block_id=doc.id,
    ).all()
