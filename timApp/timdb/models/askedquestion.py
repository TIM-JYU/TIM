import json
from datetime import timedelta, datetime
from typing import Optional

from timApp.timdb.models.askedjson import AskedJson
from timApp.timdb.models.lecture import Lecture
from timApp.timdb.tim_models import db


class AskedQuestion(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'askedquestion'
    asked_id = db.Column(db.Integer, primary_key=True)
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'), nullable=False)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    par_id = db.Column(db.Text)
    asked_time = db.Column(db.DateTime(timezone=True), nullable=False)
    points = db.Column(db.Text)  # not a single number; cannot be numeric
    asked_json_id = db.Column(db.Integer, db.ForeignKey('askedjson.asked_json_id'), nullable=False)
    expl = db.Column(db.Text)

    asked_json: AskedJson = db.relationship('AskedJson', back_populates='asked_questions', lazy='joined')
    lecture: Lecture = db.relationship('Lecture', back_populates='asked_questions', lazy='joined')
    answers = db.relationship('LectureAnswer', back_populates='asked_question', lazy='dynamic')

    @property
    def end_time(self) -> Optional[datetime]:
        timelimit = self.time_limit
        if not timelimit:
            return None
        return self.asked_time + timedelta(seconds=timelimit)

    @property
    def time_limit(self):
        return self.asked_json.to_json()['json'].get('timeLimit')

    def to_json(self):
        aj = self.asked_json.to_json()
        if self.expl:
            aj['json']['expl'] = json.loads(self.expl)
        if self.points:
            aj['json']['points'] = self.points
        return {
            'asked_id': self.asked_id,
            'json': aj,
            'asked_time': self.asked_time,
            'doc_id': self.doc_id,
            'lecture_id': self.lecture_id,
            'par_id': self.par_id,
        }


def get_asked_question(asked_id: int) -> Optional[AskedQuestion]:
    return AskedQuestion.query.get(asked_id)
