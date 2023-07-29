import json
from contextlib import contextmanager
from datetime import timedelta, datetime
from typing import Optional, TYPE_CHECKING, List

from sqlalchemy import func, select
from sqlalchemy.orm import mapped_column, Mapped, DynamicMapped

from timApp.lecture.question_utils import qst_rand_array, qst_filter_markup_points
from timApp.lecture.questionactivity import QuestionActivityKind, QuestionActivity
from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz
from timApp.util.utils import get_current_time

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.lecture.askedjson import AskedJson
    from timApp.lecture.lecture import Lecture
    from timApp.lecture.lectureanswer import LectureAnswer
    from timApp.lecture.runningquestion import Runningquestion
    from timApp.lecture.showpoints import Showpoints


class AskedQuestion(db.Model):
    __tablename__ = "askedquestion"

    asked_id: Mapped[int] = mapped_column(primary_key=True)
    lecture_id: Mapped[int] = mapped_column(db.ForeignKey("lecture.lecture_id"))
    doc_id: Mapped[Optional[int]] = mapped_column(db.ForeignKey("block.id"))
    par_id: Mapped[Optional[str]]
    asked_time: Mapped[datetime_tz]
    points: Mapped[Optional[str]]
    asked_json_id: Mapped[int] = mapped_column(db.ForeignKey("askedjson.asked_json_id"))
    expl: Mapped[Optional[str]]

    asked_json: Mapped["AskedJson"] = db.relationship(
        back_populates="asked_questions", lazy="selectin"
    )
    lecture: Mapped["Lecture"] = db.relationship(
        back_populates="asked_questions", lazy="selectin"
    )
    answers: DynamicMapped["LectureAnswer"] = db.relationship(
        back_populates="asked_question", lazy="dynamic"
    )
    answers_all: Mapped[List["LectureAnswer"]] = db.relationship(
        back_populates="asked_question", overlaps="answers"
    )
    running_question: Mapped[Optional["Runningquestion"]] = db.relationship(
        back_populates="asked_question", lazy="select"
    )
    questionactivity: DynamicMapped["QuestionActivity"] = db.relationship(
        back_populates="asked_question", lazy="dynamic"
    )
    showpoints: Mapped[Optional["Showpoints"]] = db.relationship(
        "Showpoints", back_populates="asked_question", lazy="select"
    )

    @property
    def end_time(self) -> datetime | None:
        timelimit = self.time_limit
        if not timelimit:
            return None
        return self.asked_time + timedelta(seconds=timelimit)

    def has_activity(self, kind: QuestionActivityKind, user: "User"):
        return self.questionactivity.filter_by(kind=kind, user_id=user.id).first()

    def add_activity(self, kind: QuestionActivityKind, user: "User"):
        if self.has_activity(kind, user):
            return
        a = QuestionActivity(kind=kind, user=user, asked_question=self)
        db.session.add(a)

    @property
    def time_limit(self):
        return self.asked_json.to_json()["json"].get("timeLimit")

    def to_json(self, hide_points=False):
        aj = self.asked_json.to_json(hide_points)
        if self.expl:
            aj["json"]["expl"] = json.loads(self.expl)
        if not hide_points and self.points:
            aj["json"]["points"] = self.points
        return {
            "asked_id": self.asked_id,
            "json": aj,
            "asked_time": self.asked_time,
            "doc_id": self.doc_id,
            "lecture_id": self.lecture_id,
            "par_id": self.par_id,
        }

    def get_effective_points(self):
        if self.points:
            return self.points
        aj = self.asked_json.to_json()
        return aj["json"].get("points")

    def get_default_points(self):
        aj = self.asked_json.to_json()
        return aj["json"].get("defaultPoints", 0)

    def build_answer_and_points(self, answer, u: "User"):
        """
        Checks whether question was randomized
        If so, set question point input accordingly and expand answer to contain randomization data
        """
        q_data = json.loads(self.asked_json.json)
        random_rows = q_data.get("randomizedRows", 0)
        if random_rows:
            row_count = len(q_data.get("rows", []))
            q_type = q_data.get("questionType")
            rand_arr = qst_rand_array(
                row_count, random_rows, str(u.id), locks=q_data.get("doNotMove")
            )
            question_points = self.get_effective_points()
            if question_points:
                question_points = qst_filter_markup_points(
                    question_points, q_type, rand_arr
                )
            # If lecture question's rows are randomized, it will be saved as a dict
            # containing additional information about how the answerer saw the question.
            # e.g {"c": [["2"]], "order": [4, 3, 5], "rows": 5, "question_type": "radio-vertical"}
            return {
                "c": answer,
                "order": rand_arr,
                "rows": row_count,
                "question_type": q_type,
            }, question_points
        return answer, self.get_effective_points()

    @property
    def is_running(self):
        rq = self.running_question
        if not rq:
            return False
        et = rq.end_time
        if not et:
            return True
        return get_current_time() < et


def get_asked_question(asked_id: int) -> AskedQuestion | None:
    return db.session.get(AskedQuestion, asked_id)


@contextmanager
def user_activity_lock(user: "User"):
    db.session.execute(select(func.pg_advisory_xact_lock(user.id)))
    yield
    return
    # db.session.query(func.pg_advisory_lock(user.id)).all()
    # try:
    #     yield
    # finally:
    #     try:
    #         r = db.session.query(func.pg_advisory_unlock(user.id)).scalar()
    #     except InvalidRequestError:
    #         db.session.rollback()
    #         r = db.session.query(func.pg_advisory_unlock(user.id)).scalar()
    #     if not r:
    #         raise Exception(f'Failed to release lock: {user.id}')
