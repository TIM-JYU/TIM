import enum

from sqlalchemy.orm import mapped_column

from timApp.item.block import BlockType
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
    AnswerAdded = 8

    @property
    def is_document_modification(self) -> bool:
        return self in (
            NotificationType.DocModified,
            NotificationType.ParAdded,
            NotificationType.ParDeleted,
            NotificationType.ParModified,
        )


class Notification(db.Model):
    """Notification settings for a User for a block."""

    __tablename__ = "notification"
    

    user_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), primary_key=True
    )
    """User id."""

    block_id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    """Item id."""

    notification_type = mapped_column(db.Enum(NotificationType), primary_key=True)
    """Notification type."""

    user = db.relationship("User", back_populates="notifications")
    block = db.relationship("Block", back_populates="notifications") # : Block

    def to_json(self) -> dict:
        j = {"type": self.notification_type}
        if is_attribute_loaded("block", self):
            if self.block.type_id == BlockType.Folder.value:
                j["item"] = self.block.folder
            elif self.block.type_id == BlockType.Document.value:
                if self.block.translation:
                    j["item"] = self.block.translation
                elif self.block.docentries:
                    j["item"] = self.block.docentries[0]
                else:
                    log_warning(f"No docentries for block: {self.block.id}")
            else:
                log_warning(
                    f"Unexpected block type in notification: {self.block.type_id} (id {self.block.id})"
                )
        return j
