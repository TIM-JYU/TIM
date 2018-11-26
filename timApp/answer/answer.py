from timApp.answer.answer_models import UserAnswer
from timApp.timdb.sqa import db, include_if_loaded
from timApp.util.utils import get_current_time


class Answer(db.Model):
    """An answer to a task."""
    __tablename__ = 'answer'
    id = db.Column(db.Integer, primary_key=True)
    """Answer identifier."""

    task_id = db.Column(db.Text, nullable=False, index=True)
    """Task id to which this answer was posted. In the form "doc_id.name", for example "2.task1"."""

    content = db.Column(db.Text, nullable=False)
    """Answer content."""

    points = db.Column(db.Float)
    """Points."""

    answered_on = db.Column(db.DateTime(timezone=True), nullable=False)
    """Answer timestamp."""

    valid = db.Column(db.Boolean, nullable=False)
    """Whether this answer is valid."""

    last_points_modifier = db.Column(db.Integer, db.ForeignKey('usergroup.id'))
    """The UserGroup who modified the points last. Null if the points have been given by the task automatically."""

    uploads = db.relationship('AnswerUpload', back_populates='answer', lazy='dynamic')
    users = db.relationship('User', secondary=UserAnswer.__table__,
                            back_populates='answers', lazy='dynamic')
    users_all = db.relationship('User', secondary=UserAnswer.__table__,
                                back_populates='answers_alt', lazy='select')
    annotations = db.relationship('Annotation', back_populates='answer')

    def __init__(self, task_id, content, points, valid, last_points_modifier=None):
        self.task_id = task_id
        self.content = content
        self.points = points
        self.valid = valid
        self.last_points_modifier = last_points_modifier
        self.answered_on = get_current_time()

    def get_answer_number(self):
        u = self.users.first()
        if not u:
            return 1
        return u.get_answers_for_task(self.task_id).filter(Answer.answered_on <= self.answered_on).count()

    def to_json(self):
        return {
            'id': self.id,
            'task_id': self.task_id,
            'content': self.content,
            'points': self.points,
            'answered_on': self.answered_on,
            'valid': self.valid,
            'last_points_modifier': self.last_points_modifier,
            **include_if_loaded('users_all', self, 'users'),
        }
