import json
from datetime import datetime, timezone
from typing import Optional, List

from timApp.timdb.tim_models import db, LectureUsers


class Lecture(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'lecture'
    lecture_id = db.Column(db.Integer, primary_key=True)
    lecture_code = db.Column(db.Text)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    lecturer = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    start_time = db.Column(db.DateTime(timezone=True), nullable=False)
    end_time = db.Column(db.DateTime(timezone=True))
    password = db.Column(db.Text)
    options = db.Column(db.Text)

    users = db.relationship('User', secondary=LectureUsers.__table__,
                            back_populates='lectures', lazy='dynamic')
    asked_questions = db.relationship('AskedQuestion', back_populates='lecture', lazy='dynamic')
    messages = db.relationship('Message', back_populates='lecture', lazy='dynamic')

    @staticmethod
    def find_by_id(lecture_id: int) -> Optional['Lecture']:
        return Lecture.query.get(lecture_id)

    @staticmethod
    def find_by_code(lecture_code: str, doc_id: int) -> Optional['Lecture']:
        return Lecture.query.filter_by(lecture_code=lecture_code, doc_id=doc_id).first()

    @staticmethod
    def get_all_in_document(doc_id: int, time: Optional[datetime]=None) -> List['Lecture']:
        if not time:
            time = datetime.min.replace(tzinfo=timezone.utc)
        return Lecture.query.filter_by(doc_id=doc_id).filter(Lecture.end_time > time).order_by(Lecture.lecture_code.asc()).all()

    @property
    def options_parsed(self):
        if not hasattr(self, '_options_parsed'):
            self._options_parsed = json.loads(self.options)
        return self._options_parsed

    @property
    def max_students(self):
        m = self.options_parsed.get('max_students')
        if m is not None:
            m = int(m)  # TODO is this needed?
        return m

    @property
    def is_full(self):
        max_students = self.max_students
        if max_students is None:
            return False
        cnt = LectureUsers.query.filter_by(lecture_id=self.lecture_id).count()
        return cnt >= max_students

    @property
    def is_running(self):
        time_now = datetime.now(timezone.utc)
        return self.start_time <= time_now < self.end_time

    def to_json(self, show_password=False):
        return {
            'lecture_id': self.lecture_id,
            'lecture_code': self.lecture_code,
            'doc_id': self.doc_id,
            'lecturer': self.lecturer,
            'start_time': self.start_time,
            'end_time': self.end_time,
            'options': self.options,
            'is_access_code': self.password != "",
        # don't expose password to client directly unless explicitly requested with the parameter
            'password': self.password if show_password else None,
            'is_full': self.is_full,
        }
