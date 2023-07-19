from sqlalchemy import func

from timApp.timdb.sqa import db


class Useractivity(db.Model):
    __tablename__ = "useractivity"
    __allow_unmapped__ = True
    
    lecture_id = db.Column(
        db.Integer, db.ForeignKey("lecture.lecture_id"), primary_key=True
    )
    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
    active = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())

    user = db.relationship("User", back_populates="useractivity", lazy="select")
    lecture = db.relationship("Lecture", back_populates="useractivity", lazy="select")
