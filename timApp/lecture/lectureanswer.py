import json
from json import JSONDecodeError
from typing import Optional, TYPE_CHECKING

from sqlalchemy import func, select, ForeignKey
from sqlalchemy.orm import lazyload, mapped_column, Mapped, relationship

from timApp.lecture.lecture import Lecture
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.user.user import User

if TYPE_CHECKING:
    from timApp.lecture.askedquestion import AskedQuestion


def unshuffle_lectureanswer(
    answer: list[list[str]], question_type: str, row_count: int, rand_arr: list[int]
) -> list[list[str]]:
    if question_type == "matrix" or question_type == "true-false":
        unshuffled_ans = [[] for x in range(row_count)]
        for i, pos in enumerate(rand_arr):
            unshuffled_ans[pos - 1] = answer[i]
    else:
        unshuffled_ans = [[]]
        for choice in answer[0]:
            unshuffled_ans[0].append(str(rand_arr[int(choice) - 1]))
    return unshuffled_ans


class LectureAnswer(db.Model):
    answer_id: Mapped[int] = mapped_column(primary_key=True)
    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    question_id: Mapped[int] = mapped_column(ForeignKey("askedquestion.asked_id"))
    lecture_id: Mapped[int] = mapped_column(ForeignKey("lecture.lecture_id"))
    answer: Mapped[str]
    answered_on: Mapped[datetime_tz]
    points: Mapped[Optional[float]]

    asked_question: Mapped["AskedQuestion"] = relationship(
        back_populates="answers", lazy="selectin"
    )
    user: Mapped["User"] = relationship(
        back_populates="lectureanswers", lazy="selectin"
    )

    @staticmethod
    def get_by_id(ans_id: int) -> Optional["LectureAnswer"]:
        return db.session.get(LectureAnswer, ans_id)

    def get_parsed_answer(self):
        # If lecture question's rows are randomized, it will be saved as a dict
        # containing additional information about how the answerer saw the question.
        # e.g {"c": [["2"]], "order": [4, 3, 5], "rows": 5, "question_type": "radio-vertical"}
        try:
            ans = json.loads(self.answer)
            if isinstance(ans, dict):
                ans = unshuffle_lectureanswer(
                    ans.get("c"),
                    ans.get("question_type"),
                    ans.get("rows"),
                    ans.get("order"),
                )
        except (JSONDecodeError, IndexError):
            ans = []
        return ans

    def to_json(self, include_question=True, include_user=True):
        ans = self.get_parsed_answer()
        result = {
            "answer": ans,
            "answer_id": self.answer_id,
            "answered_on": self.answered_on,
            "points": self.points,
        }
        if include_question:
            result["asked_question"] = self.asked_question
        else:
            result["asked_id"] = self.question_id
        if include_user:
            result["user"] = self.user
        else:
            result["user_id"] = self.user_id
        return result


def get_totals(
    lecture: Lecture, user: User | None = None
) -> list[tuple[User, float, int]]:
    stmt = select(User)
    if user:
        stmt = stmt.filter_by(id=user.id)
    stmt = (
        stmt.join(LectureAnswer)
        .options(lazyload(User.groups))
        .filter_by(lecture_id=lecture.lecture_id)
        .group_by(User.id)
        .order_by(User.name)
        .with_only_columns(User, func.sum(LectureAnswer.points), func.count())
    )
    return run_sql(stmt).all()
