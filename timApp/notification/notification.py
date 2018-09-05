import enum

from timApp.item.block import Block
from timApp.timdb.sqa import db, is_attribute_loaded


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

    # TODO: The name should be item_id because it's not just documents.
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    """Item id."""

    email_doc_modify = db.Column(db.Boolean, nullable=False, default=False)
    """Whether to get emails from document modifications."""

    email_comment_add = db.Column(db.Boolean, nullable=False, default=False)
    """Whether to get emails from new comments."""

    email_comment_modify = db.Column(db.Boolean, nullable=False, default=False)
    """Whether to get emails from edited comments."""

    user = db.relationship('User', back_populates='notifications')
    block: Block = db.relationship('Block', back_populates='notifications')

    def to_json(self):
        j = {
            'email_doc_modify': self.email_doc_modify,
            'email_comment_add': self.email_comment_add,
            'email_comment_modify': self.email_comment_modify,
        }
        if is_attribute_loaded('block', self):
            # Assuming block.folder, block.docentries have also been loaded.
            j['item'] = self.block.folder or self.block.translation or self.block.docentries[0]
        return j
