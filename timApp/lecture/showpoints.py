from timApp.timdb.sqa import db


class Showpoints(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'showpoints'
    asked_id = db.Column(db.Integer, db.ForeignKey('askedquestion.asked_id'), primary_key=True)

    asked_question = db.relationship('AskedQuestion', back_populates='showpoints', lazy='select')