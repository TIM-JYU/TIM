from collections import defaultdict
from datetime import datetime
from random import shuffle
from typing import DefaultDict

from timApp.answer.answer import Answer
from timApp.answer.answers import get_points_by_rule, get_latest_valid_answers_query
from timApp.document.docinfo import DocInfo
from timApp.peerreview.peerreview import PeerReview
from timApp.plugin.plugin import Plugin
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember, membership_current
from timApp.util.utils import get_current_time


def generate_review_groups(doc: DocInfo, task_ids: list[TaskId]) -> None:
    settings = doc.document.get_settings()
    user_groups = settings.groups()
    user_ids = None
    if user_groups:
        user_ids = [
            uid
            for uid, in (
                UserGroupMember.query.join(UserGroup, UserGroupMember.group)
                .filter(membership_current & (UserGroup.name.in_(user_groups)))
                .with_entities(UserGroupMember.user_id)
                .all()
            )
        ]

    points = get_points_by_rule(None, task_ids, user_ids)

    users = []
    for user in points:
        users.append(user["user"])

    shuffle(users)
    review_count = settings.peer_review_count()
    start_time_reviews: datetime = settings.peer_review_start() or get_current_time()
    end_time_reviews: datetime = settings.peer_review_stop() or get_current_time()

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

    if review_count >= len(users):
        raise PeerReviewException(
            "peer_review_count setting is too high to form review groups "
            f"(set to {review_count} but {len(users)} users have answered so far)"
        )

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

    # Current logic:
    # Everyone who answered to any task on the given list of tasks will be accepted into review
    # PeerReview rows and pairings will be the same for every task, even if target did not answer to some of tasks
    # If target has an answer in a task, try to add it to PeerReview table. If not, just leave it empty
    for t in task_ids:
        answers: list[Answer] = get_latest_valid_answers_query(t, users).all()
        excluded_users: list[User] = []
        filtered_answers = []
        for answer in answers:
            if len(answer.users_all) > 1:
                # TODO: Implement handling for multiple users
                continue
            else:
                if answer.users_all[0] not in excluded_users:
                    filtered_answers.append(answer)
                    excluded_users.append(answer.users_all[0])
        for reviewer, reviewables in pairing.items():
            for reviewable in reviewables:
                saved_with_answer = False
                for a in filtered_answers:
                    if a.users_all[0].id == reviewable:
                        save_review_by_answer(
                            a, t, doc, reviewer, start_time_reviews, end_time_reviews
                        )
                        saved_with_answer = True
                        break
                if not saved_with_answer:
                    save_review(
                        None,
                        t,
                        doc,
                        reviewer,
                        reviewable,
                        start_time_reviews,
                        end_time_reviews,
                    )

    db.session.commit()


class PeerReviewException(Exception):
    pass


def save_review(
    answer: Answer | None,
    task_id: TaskId | None,
    doc: DocInfo,
    reviewer_id: int,
    reviewable_id: int,
    start_time: datetime,
    end_time: datetime,
    reviewed: bool = False,
) -> PeerReview:
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


def save_review_by_answer(
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
