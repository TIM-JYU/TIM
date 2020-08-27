""""""
from datetime import datetime
from typing import List, Optional, Dict

from sqlalchemy.orm import Query

from timApp.answer.answer import Answer
from timApp.answer.answers import get_points_by_rule, get_latest_valid_answers_query
from timApp.document.document import Document
from timApp.plugin.plugin import Plugin
from timApp.plugin.taskid import TaskId
from timApp.peerreview.peerreview import PeerReview
from timApp.timdb.sqa import db
from timApp.user.user import User

ERRORMESSAGE_PAIRS = "Won't work, not enough users to form pairs"
ERRORMESSAGE_COUNTLOW = "Won't work, review count too low."

# Generate reviews groups for list
def generate_review_groups(doc: Document, tasks: List[Plugin]) -> None:
    task_ids = []

    for task in tasks:
        if task.task_id:
            task_ids.append(task.task_id)

    points = get_points_by_rule(None, task_ids, None)

    users = []
    for user in points:
        users.append(user.get("user"))

    settings = doc.get_settings()
    review_count: int = settings.get("peer_review_count", 1)

    # TODO: [Kuvio] get timestamps from doc settings
    start_time_reviews = datetime.now()
    end_time_reviews = datetime.now()

    # Dictionary containing review pairs, 
    # has reviewer user ID as key and value is list containing reviewable user IDs
    pairing: Dict[int, List[int]] = {}

    # TODO: [Kuvio] Implement proper handling
    if len(users) < 2:
        print(ERRORMESSAGE_PAIRS)

    # TODO: [Kuvio] Implement proper handling
    if review_count == 0:
        print(ERRORMESSAGE_COUNTLOW)

    # Decreases review count if it exceeds available user count
    if review_count > len(users):
        review_count = len(users) - 1

    # Initializes pair lists for each user
    for x in range(0, len(users)):
        pairing[users[x].id] = []

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
        answers: List[Answer] = get_latest_valid_answers_query(t, users).all()
        excluded_users: List[User] = []
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

        for reviewer in pairing.keys():
            for a in filtered_answers:
                if a.users_all[0].id in pairing[reviewer]:
                    save_review(a, t, doc, reviewer, start_time_reviews, end_time_reviews, False)

    db.session.commit()


def save_review(answer: Answer,
                task_id: TaskId,
                doc: Document,
                reviewer_id: int,
                start_time: datetime,
                end_time: datetime,
                reviewed: bool = False) -> PeerReview:
    """Saves a review to the database.

    :param answer: Answer object for answer id and reviewed user.
    :param task_id: TaskId object to provide task name.
    :param doc: Document containing reviewable answers.
    :param reviewer_id: User ID for the reviewer user.
    :param start_time: Timestamp for starting the review period.
    :param end_time: Timestamp for ending the review period.
    :param reviewed: Boolean indicating if review has been done.

    """
    review = PeerReview(
        answer_id=answer.id,
        task_name=task_id.task_name,
        block_id=doc.doc_id,
        reviewer_id=reviewer_id,
        reviewable_id=answer.users_all[0].id,
        start_time=start_time,
        end_time=end_time,
        reviewed=reviewed
    )

    db.session.add(review)
    return review


def get_reviews_for_user(d: Document, user: int) -> Query:
    q = PeerReview.query.filter_by(block_id=d.doc_id, reviewer_id=user).with_entities(PeerReview)
    return q


def check_review_access(doc: Document, current_user: int, task_id: TaskId, reviewable_user: Optional[User]) -> bool:
    if reviewable_user is None:
        reviews = PeerReview.query.filter_by(block_id=doc.doc_id, reviewer_id=current_user, task_name=task_id.task_name).with_entities(PeerReview).all()
    else:
        reviews = PeerReview.query.filter_by(block_id=doc.doc_id, reviewer_id=current_user, task_name=task_id.task_name, reviewable_id=reviewable_user.id).with_entities(PeerReview).all()
    if reviews:
        return True
    else:
        return False


def check_review_grouping(doc: Document) -> bool:
    grouping_list = PeerReview.query.filter_by(block_id=doc.doc_id).with_entities(PeerReview).all()
    if grouping_list:
        return True
    else:
        return False
