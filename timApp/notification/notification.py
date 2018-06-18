import enum

from timApp.timdb.sqa import db


class NotificationType(enum.Enum):
    DocModified = 'doc_modify'
    CommentAdded = 'comment_add'
    CommentModified = 'comment_modify'


class Notification(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'notification'
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    email_doc_modify = db.Column(db.Boolean, nullable=False, default=False)
    email_comment_add = db.Column(db.Boolean, nullable=False, default=False)
    email_comment_modify = db.Column(db.Boolean, nullable=False, default=False)

    user = db.relationship('User', back_populates='notifications')

    def __json__(self):
        return ['email_doc_modify', 'email_comment_add', 'email_comment_modify']
