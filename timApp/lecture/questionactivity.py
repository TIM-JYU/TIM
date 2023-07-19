from enum import Enum

from timApp.timdb.sqa import db


class QuestionActivityKind(Enum):
    Pointsclosed = 1
    Pointsshown = 2
    Useranswered = 3
    Userextended = 4  # Not used anymore.
    Usershown = 5


class QuestionActivity(db.Model):
    __tablename__ = "question_activity"
    __allow_unmapped__ = True
    
    asked_id = db.Column(
        db.Integer, db.ForeignKey("askedquestion.asked_id"), primary_key=True
    )
    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
    kind = db.Column(db.Enum(QuestionActivityKind), primary_key=True)

    asked_question = db.relationship(
        "AskedQuestion", back_populates="questionactivity", lazy="select"
    )
    user = db.relationship("User", back_populates="questionactivity", lazy="select")
