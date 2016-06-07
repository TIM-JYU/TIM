import datetime

from tim_app import db


class UserGroup(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'UserGroup'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String)


class Answer(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'Answer'
    id = db.Column(db.Integer, primary_key=True)
    task_id = db.Column(db.String)
    points = db.Column(db.String)
    answered_on = db.Column(db.TIMESTAMP)
    valid = db.Column(db.Boolean)


class Block(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'Block'
    id = db.Column(db.Integer, primary_key=True)
    latest_revision_id = db.Column(db.Integer)
    type_id = db.Column(db.Integer)
    description = db.Column(db.String)
    created = db.Column(db.TIMESTAMP)
    modified = db.Column(db.TIMESTAMP)
    UserGroup_id = db.Column(db.Integer, db.ForeignKey('UserGroup.id'))

    owner = db.relationship('UserGroup', backref=db.backref('owned_blocks', lazy='dynamic'))

    def __init__(self, type_id, usergroup_id, description=None):
        self.type_id = type_id
        self.UserGroup_id = usergroup_id
        self.description = description
        self.created = datetime.datetime.now()


class AnswerUpload(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'AnswerUpload'
    upload_block_id = db.Column(db.Integer, db.ForeignKey('Block.id'), primary_key=True)
    answer_id = db.Column(db.Integer, db.ForeignKey('Answer.id'))

    block = db.relationship('Block', backref=db.backref('answerupload', lazy='dynamic'))
    answer = db.relationship('Answer', backref=db.backref('uploads', lazy='dynamic'))

    def __init__(self, block, answer=None):
        self.block = block
        self.answer = answer
