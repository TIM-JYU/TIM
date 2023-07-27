from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db


class LectureUsers(db.Model):
    __tablename__ = "lectureusers"

    lecture_id: Mapped[int] = mapped_column(db.ForeignKey("lecture.lecture_id"), primary_key=True)
    user_id: Mapped[int] = mapped_column(db.ForeignKey("useraccount.id"), primary_key=True)
