from typing import TYPE_CHECKING

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db

if TYPE_CHECKING:
    from timApp.lecture.askedquestion import AskedQuestion


class ShowPoints(db.Model):
    asked_id: Mapped[int] = mapped_column(
        ForeignKey("askedquestion.asked_id"), primary_key=True
    )

    asked_question: Mapped["AskedQuestion"] = relationship(
        back_populates="showpoints", lazy="select"
    )
