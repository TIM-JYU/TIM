from datetime import datetime

from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class Runningquestion(db.Model):
    
    
    asked_id = mapped_column(
        db.Integer, db.ForeignKey("askedquestion.asked_id"), primary_key=True
    )
    lecture_id = mapped_column(
        db.Integer, db.ForeignKey("lecture.lecture_id"), primary_key=True
    )  # TODO should not be part of primary key (asked_id is enough)
    ask_time = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )
    end_time = mapped_column(db.DateTime(timezone=True))

    asked_question = db.relationship(
        "AskedQuestion", back_populates="running_question", lazy="select"
    ) # : AskedQuestion
    lecture = db.relationship(
        "Lecture", back_populates="running_questions", lazy="select"
    )
