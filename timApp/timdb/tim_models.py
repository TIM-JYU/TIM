"""Defines the persistent data models used by TIM.

Each model MUST have 'tim_main' as the __bind_key__ attribute.

__tablename__ is not mandatory but recommended in order to maintain the naming convention for tables. The default table
name is class name in lowercase.

Keep the model classes in alphabetical order.

Use Flask-Migrate for database migrations. See <http://flask-migrate.readthedocs.io/en/latest/>.

"""
import datetime
import json
from datetime import timezone

from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import func

from timApp.timdb.readparagraphtype import ReadParagraphType

db = SQLAlchemy()


class AccessType(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'accesstype'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False)


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

    def __init__(self, task_id, content, points, valid, last_points_modifier=None):
        self.task_id = task_id
        self.content = content
        self.points = points
        self.valid = valid
        self.last_points_modifier = last_points_modifier
        self.answered_on = datetime.datetime.now(timezone.utc)


class AnswerTag(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'answertag'
    id = db.Column(db.Integer, primary_key=True)
    answer_id = db.Column(db.Integer, db.ForeignKey('answer.id'), nullable=False)
    tag = db.Column(db.Text, nullable=False)


class AnswerUpload(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'answerupload'
    upload_block_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    answer_id = db.Column(db.Integer, db.ForeignKey('answer.id'))

    block = db.relationship('Block', backref=db.backref('answerupload', lazy='dynamic'))
    answer = db.relationship('Answer', backref=db.backref('uploads', lazy='dynamic'))

    def __init__(self, block, answer=None):
        self.block = block
        self.answer = answer


class AskedJson(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'askedjson'
    asked_json_id = db.Column(db.Integer, primary_key=True)
    json = db.Column(db.Text, nullable=False)
    hash = db.Column(db.Text, nullable=False)


class AskedQuestion(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'askedquestion'
    asked_id = db.Column(db.Integer, primary_key=True)
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'), nullable=False)  # NOTE Added foreign key
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'))  # NOTE Added foreign key
    par_id = db.Column(db.Text)
    asked_time = db.Column(db.DateTime(timezone=True), nullable=False)
    points = db.Column(db.Text)  # TODO Should possibly be numeric
    asked_json_id = db.Column(db.Integer, db.ForeignKey('askedjson.asked_json_id'), nullable=False)
    expl = db.Column(db.Text)


class BlockAccess(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'blockaccess'
    block_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), primary_key=True)
    type = db.Column(db.Integer, db.ForeignKey('accesstype.id'), primary_key=True)
    accessible_from = db.Column(db.DateTime(timezone=True))
    accessible_to = db.Column(db.DateTime(timezone=True))
    duration = db.Column(db.Interval)
    duration_from = db.Column(db.DateTime(timezone=True))
    duration_to = db.Column(db.DateTime(timezone=True))

    block = db.relationship('Block', backref=db.backref('accesses', lazy='dynamic'))
    usergroup = db.relationship('UserGroup', backref=db.backref('accesses', lazy='dynamic'))

    @property
    def future(self):
        return self.accessible_from is not None and datetime.datetime.now(tz=timezone.utc) < self.accessible_from

    @property
    def expired(self):
        return self.accessible_to is not None and datetime.datetime.now(tz=timezone.utc) > self.accessible_to

    @property
    def unlockable(self):
        return self.accessible_from is None and self.duration is not None and not self.duration_future and not self.duration_expired

    @property
    def duration_future(self):
        return self.duration_from and datetime.datetime.now(tz=timezone.utc) < self.duration_from

    @property
    def duration_expired(self):
        return self.duration_to and datetime.datetime.now(tz=timezone.utc) >= self.duration_to

    @property
    def seconds_left(self):
        if self.accessible_to is None:
            return None
        return (self.accessible_to - datetime.datetime.now(tz=timezone.utc)).total_seconds()

    def __hash__(self):
        return hash((self.block_id,
                     self.usergroup_id,
                     self.type,
                     self.accessible_from,
                     self.accessible_to,
                     self.duration,
                     self.duration_from,
                     self.duration_to))

    def __eq__(self, other: 'BlockAccess'):
        return self.block_id == other.block_id and self.usergroup_id == other.usergroup_id and self.type == other.type

    def __ne__(self, other):
        return not self == other


class Lecture(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'lecture'
    lecture_id = db.Column(db.Integer, primary_key=True)
    lecture_code = db.Column(db.Text)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)  # NOTE Added foreign key
    lecturer = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)  # NOTE Added foreign key
    start_time = db.Column(db.DateTime(timezone=True), nullable=False)
    end_time = db.Column(db.DateTime(timezone=True))
    password = db.Column(db.Text)
    options = db.Column(db.Text)

    @staticmethod
    def find_by_id(lecture_id: int) -> 'Lecture':
        return Lecture.query.get(lecture_id)

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

    def to_json(self, show_password=False):
        return {
            'lecture_id': self.lecture_id,
            'lecture_code': self.lecture_code,
            'doc_id': self.doc_id,
            'lecturer': self.lecturer,
            'start_time': self.start_time,
            'end_time': self.end_time,
            'options': self.options,
            'is_access_code': self.password != "",  # don't expose password to client directly unless explicitly requested with the parameter
            'password': self.password if show_password else None,
            'is_full': self.is_full,
        }


class LectureAnswer(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'lectureanswer'
    answer_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)  # NOTE Added foreign key
    question_id = db.Column(db.Integer, db.ForeignKey('askedquestion.asked_id'),
                            nullable=False)  # NOTE Added foreign key
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'), nullable=False)  # NOTE Added foreign key
    answer = db.Column(db.Text, nullable=False)
    answered_on = db.Column(db.DateTime(timezone=True), nullable=False)
    points = db.Column(db.Float)


class LectureUsers(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'lectureusers'
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'), primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'),
                        primary_key=True)  # NOTE The foreign key was wrong in schema2


class Message(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'message'
    msg_id = db.Column(db.Integer, primary_key=True)
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'),
                           nullable=False)  # NOTE The foreign key was wrong in schema2
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'),
                        nullable=False)  # NOTE The foreign key was wrong in schema2
    message = db.Column(db.Text, nullable=False)
    timestamp = db.Column(db.DateTime(timezone=True), nullable=False)


class Question(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'question'
    question_id = db.Column(db.Integer, primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)  # NOTE Added foreign key
    par_id = db.Column(db.Text, nullable=False)
    question_title = db.Column(db.Text, nullable=False)
    answer = db.Column(db.Text)
    questionjson = db.Column(db.Text)
    points = db.Column(db.Text)
    expl = db.Column(db.Text)


class ReadParagraph(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'readparagraphs'
    usergroup_id = db.Column(db.Integer, primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)  # NOTE Added foreign key
    par_id = db.Column(db.Text, primary_key=True)
    type = db.Column(db.Enum(ReadParagraphType), nullable=False, primary_key=True)
    par_hash = db.Column(db.Text, nullable=False)
    timestamp = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    __table_args__ = (db.Index('readparagraphs_doc_id_par_id_idx', 'doc_id', 'par_id'),)


class UserAnswer(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'useranswer'
    id = db.Column(db.Integer, primary_key=True)
    answer_id = db.Column(db.Integer, db.ForeignKey('answer.id'), nullable=False)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    __table_args__ = (db.UniqueConstraint('answer_id', 'user_id'),)


class UserGroupMember(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'usergroupmember'
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)


# UserGroupMember = db.Table('usergroupmember',
#                            db.Column('usergroup_id', db.Integer, db.ForeignKey('usergroup.id'), primary_key=True),
#                            db.Column('user_id', db.Integer, db.ForeignKey('useraccount.id'), primary_key=True),
#                            info={'bind_key': 'tim_main'}
#                            )


class UserNotes(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'usernotes'
    id = db.Column(db.Integer, primary_key=True)
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), nullable=False)  # NOTE Added foreign key
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)  # NOTE Added foreign key
    par_id = db.Column(db.Text, nullable=False)
    par_hash = db.Column(db.Text, nullable=False)
    content = db.Column(db.Text, nullable=False)
    created = db.Column(db.DateTime(timezone=True), nullable=False)
    modified = db.Column(db.DateTime(timezone=True))
    access = db.Column(db.Text, nullable=False)
    tags = db.Column(db.Text, nullable=False)
    html = db.Column(db.Text)

