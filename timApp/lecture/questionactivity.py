from enum import Enum
from typing import TYPE_CHECKING

from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db

if TYPE_CHECKING:
    from timApp.lecture.askedquestion import AskedQuestion
    from timApp.user.user import User

class QuestionActivityKind(Enum):
    Pointsclosed = 1
    Pointsshown = 2
    Useranswered = 3
    Userextended = 4  # Not used anymore.
    Usershown = 5


class QuestionActivity(db.Model):
    __tablename__ = "question_activity"

    asked_id: Mapped[int] = mapped_column(db.ForeignKey("askedquestion.asked_id"), primary_key=True)
    user_id: Mapped[int] = mapped_column(
        db.ForeignKey("useraccount.id"), primary_key=True
    )
    kind: Mapped[QuestionActivityKind] = mapped_column(primary_key=True)

    asked_question: Mapped["AskedQuestion"] = db.relationship(back_populates="questionactivity", lazy="select"
    )
    user: Mapped["User"] = db.relationship(back_populates="questionactivity", lazy="select")
