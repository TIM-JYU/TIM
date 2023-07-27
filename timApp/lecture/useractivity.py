from typing import TYPE_CHECKING

from sqlalchemy import func
from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.lecture.lecture import Lecture

class Useractivity(db.Model):
    __tablename__ = "useractivity"
    

    lecture_id: Mapped[int] = mapped_column(
        db.ForeignKey("lecture.lecture_id"), primary_key=True
    )
    user_id: Mapped[int] = mapped_column(
        db.ForeignKey("useraccount.id"), primary_key=True
    )
    active: Mapped[datetime_tz] = mapped_column(default=func.now()
    )

    user: Mapped["User"] = db.relationship(back_populates="useractivity", lazy="select")
    lecture: Mapped["Lecture"] = db.relationship(back_populates="useractivity", lazy="select")
