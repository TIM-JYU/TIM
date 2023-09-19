from datetime import datetime
from enum import Enum
from typing import Any, Optional, TYPE_CHECKING, List

from sqlalchemy import func, select, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import run_sql, db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.item.block import Block
    from timApp.user.usergroup import UserGroup


class DisplayType(Enum):
    TOP_OF_PAGE = 1
    STICKY = 2


class InternalMessage(db.Model):
    """A TIM message."""

    __tablename__ = "internalmessage"

    id: Mapped[int] = mapped_column(primary_key=True)
    """Message identifier."""

    created: Mapped[datetime_tz] = mapped_column(default=func.now())
    """Date and time when the message was created."""

    doc_id: Mapped[int] = mapped_column(ForeignKey("block.id"))
    """Block identifier."""

    par_id: Mapped[str]
    """Paragraph identifier."""

    can_mark_as_read: Mapped[bool]
    """Whether the recipient can mark the message as read."""

    reply: Mapped[bool]
    """Whether the message can be replied to."""

    display_type: Mapped[DisplayType]
    """How the message is displayed."""

    expires: Mapped[Optional[datetime]]
    """"When the message display will disappear."""

    replies_to: Mapped[Optional[int]]
    """Id of the message which this messages is a reply to"""

    displays: Mapped[List["InternalMessageDisplay"]] = relationship(
        back_populates="message"
    )
    readreceipts: Mapped[List["InternalMessageReadReceipt"]] = relationship(
        back_populates="message"
    )
    block: Mapped["Block"] = relationship(back_populates="internalmessage")

    def to_json(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "created": self.created,
            "doc_id": self.doc_id,
            "par_id": self.par_id,
            "can_mark_as_read": self.can_mark_as_read,
            "reply": self.reply,
            "display_type": self.display_type,
            "displays": self.displays,
            "readreceipts": self.readreceipts,
        }


class InternalMessageDisplay(db.Model):
    """Where and for whom a TIM message is displayed."""

    __tablename__ = "internalmessage_display"

    id: Mapped[int] = mapped_column(primary_key=True)
    """Message display identifier."""

    message_id: Mapped[int] = mapped_column(ForeignKey("internalmessage.id"))
    """Message identifier."""

    usergroup_id: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    """Who sees the message; if null, displayed for everyone."""

    display_doc_id: Mapped[Optional[int]] = mapped_column(ForeignKey("block.id"))
    """
    Identifier for the document or the folder where the message is displayed. If null, 
    the message is displayed globally.
    """

    message: Mapped["InternalMessage"] = relationship(back_populates="displays")
    usergroup: Mapped[Optional["UserGroup"]] = relationship(
        back_populates="internalmessage_display"
    )
    display_block: Mapped[Optional["Block"]] = relationship(
        back_populates="internalmessage_display"
    )

    def to_json(self) -> dict[str, Any]:
        return {
            "id": self.id,
            "message_id": self.message_id,
            "usergroup_id": self.usergroup_id,
            "display_doc_id": self.display_doc_id,
        }


class InternalMessageReadReceipt(db.Model):
    """Metadata about read receipts."""

    __tablename__ = "internalmessage_readreceipt"

    message_id: Mapped[int] = mapped_column(
        ForeignKey("internalmessage.id"), primary_key=True
    )
    """Message identifier."""

    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), primary_key=True)
    """Identifier for the user who marked the message as read."""

    last_seen: Mapped[Optional[datetime]]
    """Timestamp for the last time the the message was displayed to the user"""

    marked_as_read_on: Mapped[Optional[datetime]]
    """Timestamp for when the message was marked as read."""

    message: Mapped["InternalMessage"] = relationship(back_populates="readreceipts")
    user: Mapped["User"] = relationship(back_populates="internalmessage_readreceipt")

    @staticmethod
    def get_for_user(
        user: "User", message: InternalMessage
    ) -> Optional["InternalMessageReadReceipt"]:
        return (
            run_sql(
                select(InternalMessageReadReceipt).filter_by(user=user, message=message)
            )
            .scalars()
            .first()
        )

    def to_json(self) -> dict[str, Any]:
        return {
            "message_id": self.message_id,
            "user_id": self.user_id,
            "marked_as_read_on": self.marked_as_read_on,
        }
