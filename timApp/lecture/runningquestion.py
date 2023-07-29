from datetime import datetime
from typing import Optional, TYPE_CHECKING

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.types import datetime_tz, DbModel

if TYPE_CHECKING:
    from timApp.lecture.askedquestion import AskedQuestion
    from timApp.lecture.lecture import Lecture


class RunningQuestion(DbModel):
    asked_id: Mapped[int] = mapped_column(
        ForeignKey("askedquestion.asked_id"), primary_key=True
    )
    lecture_id: Mapped[int] = mapped_column(
        ForeignKey("lecture.lecture_id"), primary_key=True
    )  # TODO should not be part of primary key (asked_id is enough)
    ask_time: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)
    end_time: Mapped[Optional[datetime_tz]]

    asked_question: Mapped["AskedQuestion"] = relationship(
        back_populates="running_question", lazy="select"
    )
    lecture: Mapped["Lecture"] = relationship(
        back_populates="running_questions", lazy="select"
    )
