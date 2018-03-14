import json
from typing import Optional

from timApp.timdb.tim_models import db, tim_main_execute
from timApp.timdb.timdbbase import result_as_dict_list


class LectureAnswer(db.Model):
    __bind_key__ = 'tim_main'
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

    def to_json(self):
        return {
            'answer': json.loads(self.answer),
            'answer_id': self.answer_id,
            'answered_on': self.answered_on,
            'asked_question': self.asked_question,
            'points': self.points,
            'user': self.user,
        }


def get_totals(lecture_id: int, user_id: Optional[int]=None):
    condition = 'AND u.id = :userid' if user_id is not None else ''
    r = tim_main_execute(f"""SELECT u.name, SUM(a.points) as sum, COUNT(*) as count
    FROM LectureAnswer a
    JOIN UserAccount u ON a.user_id = u.id
    WHERE a.lecture_id = :lectureid
    {condition}
    GROUP BY u.name
    ORDER BY u.name""", {'lectureid': lecture_id, **({'userid': user_id} if user_id else {})})
    return result_as_dict_list(r.cursor)
