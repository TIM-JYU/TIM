import enum

from timApp.item.block import Block, BlockType
from timApp.timdb.sqa import db, is_attribute_loaded
from timApp.util.logger import log_warning


class NotificationType(enum.Enum):
    DocModified = 1
    ParAdded = 2
    ParModified = 3
    ParDeleted = 4
    CommentAdded = 5
    CommentModified = 6
    CommentDeleted = 7

    @property
    def is_document_modification(self):
        return self in (
            NotificationType.DocModified,
            NotificationType.ParAdded,
            NotificationType.ParDeleted,
            NotificationType.ParModified,
        )


class Notification(db.Model):
    """Notification settings for a User for a document.

    TODO: Instead of columns email_*, there should just be one column notify_kind with type NotificationType and
    TODO: it would be part of primary key.
    """
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
            if self.block.type_id == BlockType.Folder.value:
                j['item'] = self.block.folder
            elif self.block.type_id == BlockType.Document.value:
                if self.block.translation:
                    j['item'] = self.block.translation
                elif self.block.docentries:
                    j['item'] = self.block.docentries[0]
                else:
                    log_warning(f'No docentries for block: {self.block.id}')
            else:
                log_warning(f'Unexpected block type in notification: {self.block.type_id} (id {self.block.id})')
        return j
