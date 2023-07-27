from datetime import datetime
from typing import Optional, TYPE_CHECKING

from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.lecture.askedquestion import AskedQuestion
    from timApp.lecture.lecture import Lecture


class Runningquestion(db.Model):
    asked_id: Mapped[int] = mapped_column(
        db.ForeignKey("askedquestion.asked_id"), primary_key=True
    )
    lecture_id: Mapped[int] = mapped_column(
        db.ForeignKey("lecture.lecture_id"), primary_key=True
    )  # TODO should not be part of primary key (asked_id is enough)
    ask_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    end_time: Mapped[Optional[datetime_tz]]

    asked_question: Mapped["AskedQuestion"] = db.relationship(
        back_populates="running_question", lazy="select"
    )
    lecture: Mapped["Lecture"] = db.relationship(
        back_populates="running_questions", lazy="select"
    )
