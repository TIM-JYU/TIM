from typing import TYPE_CHECKING

from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db

if TYPE_CHECKING:
    from timApp.lecture.askedquestion import AskedQuestion

class Showpoints(db.Model):
    __tablename__ = "showpoints"
    

    asked_id: Mapped[int] = mapped_column(db.ForeignKey("askedquestion.asked_id"), primary_key=True
    )

    asked_question: Mapped["AskedQuestion"] = db.relationship(back_populates="showpoints", lazy="select"
    )
