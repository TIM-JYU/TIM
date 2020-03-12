import json
from json import JSONDecodeError
from typing import Optional, List, Tuple

from sqlalchemy import func
from sqlalchemy.orm import lazyload

from timApp.lecture.lecture import Lecture
from timApp.timdb.sqa import db
from timApp.user.user import User


class LectureAnswer(db.Model):
    __tablename__ = 'lectureanswer'
    answer_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    question_id = db.Column(db.Integer, db.ForeignKey('askedquestion.asked_id'),
                            nullable=False)
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'), nullable=False)
    answer = db.Column(db.Text, nullable=False)
    answered_on = db.Column(db.DateTime(timezone=True), nullable=False)
    points = db.Column(db.Float)

    asked_question = db.relationship('AskedQuestion', back_populates='answers', lazy='joined')
    user = db.relationship('User', back_populates='lectureanswers', lazy='joined')

    @staticmethod
    def get_by_id(ans_id: int) -> Optional['LectureAnswer']:
        return LectureAnswer.query.get(ans_id)

    def to_json(self, include_question=True, include_user=True):
        try:
            ans = json.loads(self.answer)
        except JSONDecodeError:
            ans = []
        result = {
            'answer': ans,
            'answer_id': self.answer_id,
            'answered_on': self.answered_on,
            'points': self.points,
        }
        if include_question:
            result['asked_question'] = self.asked_question
        else:
            result['asked_id'] = self.question_id
        if include_user:
            result['user'] = self.user
        else:
            result['user_id'] = self.user_id
        return result


def get_totals(lecture: Lecture, user: Optional[User]=None) -> List[Tuple[User, float, int]]:
    q = User.query
    if user:
        q = q.filter_by(id=user.id)
    q = (q.join(LectureAnswer)
         .options(lazyload(User.groups))
         .filter_by(lecture_id=lecture.lecture_id)
         .group_by(User.id)
         .order_by(User.name)
         .with_entities(User,
                        func.sum(LectureAnswer.points),
                        func.count())
         )
    return q.all()
