from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy.orm import Mapped, mapped_column

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz


class Badge(db.Model):
    """
    A badge that can be given to a user.
    """

    __tablename__ = "badge"

    id: Mapped[int] = mapped_column(primary_key=True, nullable=False)
    """Identifier of the badge"""

    title: Mapped[str] = mapped_column(nullable=False)
    """Title of the badge"""

    description: Mapped[str] = mapped_column(nullable=False)
    """Description of the badge"""

    color: Mapped[str] = mapped_column(nullable=False)
    """Color of the badge"""

    shape: Mapped[str] = mapped_column(nullable=False)
    """Shape of the badge"""

    image: Mapped[int] = mapped_column(nullable=False)
    """Image of the badge"""

    context_group: Mapped[int] = mapped_column(
        ForeignKey("usergroup.id"), nullable=False
    )
    """Context group where the badge belongs to"""

    active: Mapped[bool] = mapped_column(nullable=False)
    """Active status of the badge. If the badge is deleted this turns to false."""

    created_by: Mapped[int] = mapped_column(
        ForeignKey("useraccount.id"), nullable=False
    )
    """Useraccount that created the badge"""

    created: Mapped[datetime_tz] = mapped_column(nullable=False)
    """Timestamp when the badge was created."""

    modified_by: Mapped[Optional[int]] = mapped_column(ForeignKey("useraccount.id"))
    """Useraccount that modified the badge"""

    modified: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was modified."""

    deleted_by: Mapped[Optional[int]] = mapped_column(ForeignKey("useraccount.id"))
    """Useraccount that deleted the badge"""

    deleted: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was deleted."""

    restored_by: Mapped[Optional[int]] = mapped_column(ForeignKey("useraccount.id"))
    """Useraccount that restored the badge"""

    restored: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was restored."""

    def to_json(self) -> dict:
        """
        Returns a json representation of the badge.
        :return: badge in json format
        """
        return {
            "id": self.id,
            "title": self.title,
            "description": self.description,
            "color": self.color,
            "shape": self.shape,
            "image": self.image,
            "context_group": self.context_group,
            "active": self.active,
            "created_by": self.created_by,
            "created": self.created,
            "modified_by": self.modified_by,
            "modified": self.modified,
            "deleted_by": self.deleted_by,
            "deleted": self.deleted,
            "restored_by": self.restored_by,
            "restored": self.restored,
        }


class BadgeGiven(db.Model):
    """
    A badge that is given to a user with an optional message.
    """

    __tablename__ = "badgegiven"

    id: Mapped[int] = mapped_column(primary_key=True, nullable=False)
    """Identifier of the given badge."""

    message: Mapped[str] = mapped_column()
    """Message of the given badge."""

    badge_id: Mapped[int] = mapped_column(ForeignKey("badge.id"), nullable=False)
    """Identifier of the badge."""

    group_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), nullable=False)
    """Identifier of the usergroup that the badge is given."""

    active: Mapped[bool] = mapped_column(nullable=False)
    """Active status of the given badge. If the badge is withdrawn this turns to false."""

    given_by: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), nullable=False)
    """Useraccount that gave the badge."""

    given: Mapped[datetime_tz] = mapped_column(nullable=False)
    """Timestamp when the badge was given."""

    withdrawn_by: Mapped[Optional[int]] = mapped_column(ForeignKey("useraccount.id"))
    """Useraccount that withdrew the badge."""

    withdrawn: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was withdrawn."""

    undo_withdrawn_by: Mapped[Optional[int]] = mapped_column(
        ForeignKey("useraccount.id")
    )
    """Useraccount that undoed the badge withdrawal."""

    undo_withdrawn: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge withdrawal was undoed."""

    def to_json(self) -> dict:
        """
        Returns a json representation of the badgegiven.
        :return: badgegiven in json format
        """
        return {
            "id": self.id,
            "message": self.message,
            "badge_id": self.badge_id,
            "group_id": self.group_id,
            "active": self.active,
            "given_by": self.given_by,
            "given": self.given,
            "withdrawn_by": self.withdrawn_by,
            "withdrawn": self.withdrawn,
            "undo_withdrawn_by": self.undo_withdrawn_by,
            "undo_withdrawn": self.undo_withdrawn,
        }
