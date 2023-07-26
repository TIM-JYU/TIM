from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class Showpoints(db.Model):
    __tablename__ = "showpoints"
    

    asked_id = mapped_column(
        db.Integer, db.ForeignKey("askedquestion.asked_id"), primary_key=True
    )

    asked_question = db.relationship(
        "AskedQuestion", back_populates="showpoints", lazy="select"
    )
