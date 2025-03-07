from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy.orm import Mapped, mapped_column, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz
from timApp.user.usergroup import UserGroup


class Badge(db.Model):
    __tablename__ = "badge"

    id: Mapped[int] = mapped_column(primary_key=True, nullable=False)
    title: Mapped[str] = mapped_column(nullable=False)
    description: Mapped[str] = mapped_column(nullable=False)
    color: Mapped[str] = mapped_column(nullable=False)
    shape: Mapped[str] = mapped_column(nullable=False)
    image: Mapped[int] = mapped_column(nullable=False)
    context_group: Mapped[str] = mapped_column(nullable=False)
    active: Mapped[bool] = mapped_column(nullable=False)
    created_by: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), nullable=False)
    created: Mapped[datetime_tz] = mapped_column(nullable=False)
    modified_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    modified: Mapped[Optional[datetime_tz]] = mapped_column()
    deleted_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    deleted: Mapped[Optional[datetime_tz]] = mapped_column()
    restored_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    restored: Mapped[Optional[datetime_tz]] = mapped_column()

    group_created: Mapped[UserGroup] = relationship(foreign_keys=created_by)
    group_modified: Mapped[Optional[UserGroup]] = relationship(foreign_keys=modified_by)
    group_deleted: Mapped[Optional[UserGroup]] = relationship(foreign_keys=deleted_by)
    group_restored: Mapped[Optional[UserGroup]] = relationship(foreign_keys=restored_by)

    def to_json(self) -> dict:
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
    __tablename__ = "badgegiven"

    id: Mapped[int] = mapped_column(primary_key=True, nullable=False)
    message: Mapped[str] = mapped_column()
    badge_id: Mapped[int] = mapped_column(ForeignKey("badge.id"), nullable=False)
    group_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), nullable=False)
    active: Mapped[bool] = mapped_column(nullable=False)
    given_by: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), nullable=False)
    given: Mapped[datetime_tz] = mapped_column(nullable=False)
    withdrawn_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    withdrawn: Mapped[Optional[datetime_tz]] = mapped_column()
    undo_withdrawn_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    undo_withdrawn: Mapped[Optional[datetime_tz]] = mapped_column()

    badge: Mapped[Badge] = relationship()
    group: Mapped[UserGroup] = relationship(foreign_keys=group_id)
    group_given: Mapped[UserGroup] = relationship(foreign_keys=given_by)
    group_withdrawn: Mapped[Optional[UserGroup]] = relationship(
        foreign_keys=withdrawn_by
    )
    group_undo_withdrawn: Mapped[Optional[UserGroup]] = relationship(
        foreign_keys=undo_withdrawn_by
    )
