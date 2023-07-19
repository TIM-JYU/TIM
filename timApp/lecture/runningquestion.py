from datetime import datetime

from timApp.lecture.askedquestion import AskedQuestion
from timApp.timdb.sqa import db


class Runningquestion(db.Model):
    __allow_unmapped__ = True
    
    asked_id = db.Column(
        db.Integer, db.ForeignKey("askedquestion.asked_id"), primary_key=True
    )
    lecture_id = db.Column(
        db.Integer, db.ForeignKey("lecture.lecture_id"), primary_key=True
    )  # TODO should not be part of primary key (asked_id is enough)
    ask_time = db.Column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    end_time = db.Column(db.DateTime(timezone=True))

    asked_question: AskedQuestion = db.relationship(
        "AskedQuestion", back_populates="running_question", lazy="select"
    )
    lecture = db.relationship(
        "Lecture", back_populates="running_questions", lazy="select"
    )
