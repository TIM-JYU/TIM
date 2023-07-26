from sqlalchemy import func
from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class Useractivity(db.Model):
    __tablename__ = "useractivity"
    

    lecture_id = mapped_column(
        db.Integer, db.ForeignKey("lecture.lecture_id"), primary_key=True
    )
    user_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), primary_key=True
    )
    active = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=func.now()
    )

    user = db.relationship("User", back_populates="useractivity", lazy="select")
    lecture = db.relationship("Lecture", back_populates="useractivity", lazy="select")
