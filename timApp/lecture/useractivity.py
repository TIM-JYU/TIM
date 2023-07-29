from typing import TYPE_CHECKING

from sqlalchemy import func, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz, DbModel

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.lecture.lecture import Lecture


class UserActivity(DbModel):
    lecture_id: Mapped[int] = mapped_column(
        ForeignKey("lecture.lecture_id"), primary_key=True
    )
    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), primary_key=True)
    active: Mapped[datetime_tz] = mapped_column(default=func.now())

    user: Mapped["User"] = relationship(back_populates="useractivity", lazy="select")
    lecture: Mapped["Lecture"] = relationship(
        back_populates="useractivity", lazy="select"
    )
