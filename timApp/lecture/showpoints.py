from timApp.timdb.sqa import db


class Showpoints(db.Model):
    __tablename__ = "showpoints"
    __allow_unmapped__ = True
    
    asked_id = db.Column(
        db.Integer, db.ForeignKey("askedquestion.asked_id"), primary_key=True
    )

    asked_question = db.relationship(
        "AskedQuestion", back_populates="showpoints", lazy="select"
    )
