import datetime
from datetime import timezone

from timApp.timdb.tim_models import db, UserAnswer


class Answer(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'answer'
    id = db.Column(db.Integer, primary_key=True)
    task_id = db.Column(db.Text, nullable=False, index=True)
    content = db.Column(db.Text, nullable=False)
    points = db.Column(db.Float)
    answered_on = db.Column(db.DateTime(timezone=True), nullable=False)
    valid = db.Column(db.Boolean, nullable=False)
    last_points_modifier = db.Column(db.Integer, db.ForeignKey('usergroup.id'))

    uploads = db.relationship('AnswerUpload', back_populates='answer', lazy='dynamic')
    users = db.relationship('User', secondary=UserAnswer.__table__,
                            back_populates='answers', lazy='dynamic')

    def __init__(self, task_id, content, points, valid, last_points_modifier=None):
        self.task_id = task_id
        self.content = content
        self.points = points
        self.valid = valid
        self.last_points_modifier = last_points_modifier
        self.answered_on = datetime.datetime.now(timezone.utc)
