"""Defines the persistent data models used by TIM.

Each model MUST have 'tim_main' as the __bind_key__ attribute.

__tablename__ is not mandatory but recommended in order to maintain the naming convention for tables. The default table
name is class name in lowercase.

Keep the model classes in alphabetical order.

Use Flask-Migrate for database migrations. See <http://flask-migrate.readthedocs.io/en/latest/>.

"""
import datetime
from datetime import timezone
from enum import Enum

from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import func

from timApp.timdb.readparagraphtype import ReadParagraphType

db = SQLAlchemy()


def tim_main_execute(sql: str, params=None):
    return db.session.execute(sql, params, bind=db.get_engine(bind='tim_main'))


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

    uploads = db.relationship('AnswerUpload', back_populates='answer', lazy='dynamic')

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

    block = db.relationship('Block', back_populates='answerupload')
    answer = db.relationship('Answer', back_populates='uploads')

    def __init__(self, block, answer=None):
        self.block = block
        self.answer = answer


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

    block = db.relationship('Block', back_populates='accesses')
    usergroup = db.relationship('UserGroup', back_populates='accesses')

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


class LectureUsers(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'lectureusers'
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'), primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'),
                        primary_key=True)


class Question(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'question'
    question_id = db.Column(db.Integer, primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    par_id = db.Column(db.Text, nullable=False)
    question_title = db.Column(db.Text, nullable=False)
    answer = db.Column(db.Text)
    questionjson = db.Column(db.Text)
    points = db.Column(db.Text)
    expl = db.Column(db.Text)


class QuestionActivityKind(Enum):
    Pointsclosed = 1
    Pointsshown = 2
    Useranswered = 3
    Userextended = 4
    Usershown = 5


class QuestionActivity(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'question_activity'
    asked_id = db.Column(db.Integer, db.ForeignKey('askedquestion.asked_id'), primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    kind = db.Column(db.Enum(QuestionActivityKind), primary_key=True)

    asked_question = db.relationship('AskedQuestion', back_populates='questionactivity', lazy='select')
    user = db.relationship('User', back_populates='questionactivity', lazy='select')


class ReadParagraph(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'readparagraph'
    id = db.Column(db.Integer, primary_key=True)
    usergroup_id = db.Column(db.Integer, nullable=False)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    par_id = db.Column(db.Text, nullable=False)
    type = db.Column(db.Enum(ReadParagraphType), nullable=False)
    par_hash = db.Column(db.Text, nullable=False)
    timestamp = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    __table_args__ = (db.Index('readparagraph_doc_id_par_id_idx', 'doc_id', 'par_id'),
                      db.Index('readparagraph_doc_id_usergroup_id_idx', 'doc_id', 'usergroup_id'),)


class Showpoints(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'showpoints'
    asked_id = db.Column(db.Integer, db.ForeignKey('askedquestion.asked_id'), primary_key=True)

    asked_question = db.relationship('AskedQuestion', back_populates='showpoints', lazy='select')


class SlideStatus(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'slide_status'
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    status = db.Column(db.Text, nullable=False)

    def __init__(self, doc_id, status):
        self.doc_id = doc_id
        self.status = status


class Useractivity(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'useractivity'
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'), primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    active = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())

    user = db.relationship('User', back_populates='useractivity', lazy='select')
    lecture = db.relationship('Lecture', back_populates='useractivity', lazy='select')


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
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), nullable=False)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    par_id = db.Column(db.Text, nullable=False)
    par_hash = db.Column(db.Text, nullable=False)
    content = db.Column(db.Text, nullable=False)
    created = db.Column(db.DateTime(timezone=True), nullable=False)
    modified = db.Column(db.DateTime(timezone=True))
    access = db.Column(db.Text, nullable=False)
    tags = db.Column(db.Text, nullable=False)
    html = db.Column(db.Text)
