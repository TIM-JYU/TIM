from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db


class LectureUsers(db.Model):
    lecture_id: Mapped[int] = mapped_column(
        ForeignKey("lecture.lecture_id"), primary_key=True
    )
    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), primary_key=True)
