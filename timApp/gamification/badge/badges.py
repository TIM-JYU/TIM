from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy.orm import Mapped, mapped_column, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz
from timApp.user.usergroup import UserGroup


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

    context_group: Mapped[str] = mapped_column(nullable=False)
    """Context group where the badge belongs to"""

    active: Mapped[bool] = mapped_column(nullable=False)
    """Active status of the badge. If the badge is deleted this turns to false."""

    created_by: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), nullable=False)
    """Usergroup that created the badge"""

    created: Mapped[datetime_tz] = mapped_column(nullable=False)
    """Timestamp when the badge was created."""

    modified_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    """Usergroup that modified the badge"""

    modified: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was modified."""

    deleted_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    """Usergroup that deleted the badge"""

    deleted: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was deleted."""

    restored_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    """Usergroup that restored the badge"""

    restored: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was restored."""

    # TODO: Duplicates? Remove these 4 relationships and do database migration.
    group_created: Mapped[UserGroup] = relationship(foreign_keys=created_by)
    group_modified: Mapped[Optional[UserGroup]] = relationship(foreign_keys=modified_by)
    group_deleted: Mapped[Optional[UserGroup]] = relationship(foreign_keys=deleted_by)
    group_restored: Mapped[Optional[UserGroup]] = relationship(foreign_keys=restored_by)

    def to_json(self) -> dict:
        """
        Returns a JSON representation of the badge.
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

    # def verify_context_group_access(self):
    #     pass


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

    given_by: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), nullable=False)
    """Usergroup that gave the badge."""

    given: Mapped[datetime_tz] = mapped_column(nullable=False)
    """Timestamp when the badge was given."""

    withdrawn_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    """Usergroup that withdrew the badge."""

    withdrawn: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge was withdrawn."""

    undo_withdrawn_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    """Usergroup that undoed the badge withdrawal."""

    undo_withdrawn: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp when the badge withdrawal was undoed."""

    # TODO: Duplicates? Remove these 5 relationships and do database migration.
    badge: Mapped[Badge] = relationship()
    group: Mapped[UserGroup] = relationship(foreign_keys=group_id)
    group_given: Mapped[UserGroup] = relationship(foreign_keys=given_by)
    group_withdrawn: Mapped[Optional[UserGroup]] = relationship(
        foreign_keys=withdrawn_by
    )
    group_undo_withdrawn: Mapped[Optional[UserGroup]] = relationship(
        foreign_keys=undo_withdrawn_by
    )
