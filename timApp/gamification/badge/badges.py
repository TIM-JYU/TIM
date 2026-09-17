from __future__ import annotations

from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy.orm import Mapped, mapped_column

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz


class BadgeTemplate(db.Model):
    """
    A badge template created by a user (usually a teacher) for a specific user group (context group).
    """

    __tablename__ = "badgetemplate"

    id: Mapped[int] = mapped_column(primary_key=True, nullable=False)
    """Identifier of the badge template"""

    title: Mapped[str] = mapped_column(nullable=False)
    """Title of the badge"""

    description: Mapped[str] = mapped_column(nullable=False)
    """Description of the badge"""

    color: Mapped[str] = mapped_column(nullable=False)
    """Color of the badge"""

    shape: Mapped[str] = mapped_column(nullable=False)
    """Shape of the badge"""

    image: Mapped[int] = mapped_column(nullable=False)
    """Image for the badge"""

    context_group: Mapped[int] = mapped_column(
        ForeignKey("usergroup.id"), nullable=False
    )
    """Context group where the badge template belongs to"""

    active: Mapped[bool] = mapped_column(nullable=False)
    """Active status of the badge template. If the badge is deleted this turns to false."""

    created_by: Mapped[int] = mapped_column(
        ForeignKey("useraccount.id"), nullable=False
    )
    """Useraccount that created the badge template"""

    created: Mapped[datetime_tz] = mapped_column(nullable=False)
    """Timestamp when the badge template was created."""

    modified: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge template was modified."""

    deleted: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge template was deleted."""

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
            "modified": self.modified,
            "deleted": self.deleted,
        }

    @staticmethod
    def get_by_id(badge_id: int) -> BadgeTemplate | None:
        return db.session.get(BadgeTemplate, badge_id)


class Badge(db.Model):
    """
    A badge that is given to a user with an optional message.
    """

    __tablename__ = "badge"

    id: Mapped[int] = mapped_column(primary_key=True, nullable=False)
    """Identifier of the badge."""

    message: Mapped[str] = mapped_column()
    """Message of the badge."""

    badge_id: Mapped[int] = mapped_column(
        ForeignKey("badgetemplate.id"), nullable=False
    )
    """Identifier of the badge template."""

    group_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), nullable=False)
    """Identifier of the usergroup that received the badge"""

    active: Mapped[bool] = mapped_column(nullable=False)
    """Active status of the badge. If the badge is withdrawn this turns to false."""

    given_by: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), nullable=False)
    """Useraccount that gave the badge."""

    given: Mapped[datetime_tz] = mapped_column(nullable=False)
    """Timestamp when the badge was given."""

    withdrawn: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was withdrawn."""

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
            "withdrawn": self.withdrawn,
        }

    @staticmethod
    def get_by_id(badge_id: int) -> Badge | None:
        return db.session.get(Badge, badge_id)
