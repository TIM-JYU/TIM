import enum

from timApp.timdb.sqa import db


class NotificationType(enum.Enum):
    DocModified = 'doc_modify'
    CommentAdded = 'comment_add'
    CommentModified = 'comment_modify'


class Notification(db.Model):
    """Notification settings for a User for a document.

    TODO: Instead of columns email_*, there should just be one column notify_kind with type NotificationType and
    TODO: it would be part of primary key.
    """
    __bind_key__ = 'tim_main'
    __tablename__ = 'notification'
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    """User id."""

    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    """Document id."""

    email_doc_modify = db.Column(db.Boolean, nullable=False, default=False)
    """Whether to get emails from document modifications."""

    email_comment_add = db.Column(db.Boolean, nullable=False, default=False)
    """Whether to get emails from new comments."""

    email_comment_modify = db.Column(db.Boolean, nullable=False, default=False)
    """Whether to get emails from edited comments."""

    user = db.relationship('User', back_populates='notifications')

    def __json__(self):
        return ['email_doc_modify', 'email_comment_add', 'email_comment_modify']
