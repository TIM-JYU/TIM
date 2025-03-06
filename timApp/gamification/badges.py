import json
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
    created_by: Mapped[Optional[int]] = mapped_column(
        ForeignKey("usergroup.id"), nullable=False
    )
    created: Mapped[datetime_tz] = mapped_column(nullable=False)
    modified_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    modified: Mapped[Optional[datetime_tz]] = mapped_column()
    deleted_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    deleted: Mapped[Optional[datetime_tz]] = mapped_column()
    restored_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    restored: Mapped[Optional[datetime_tz]] = mapped_column()

    group_created: Mapped[Optional[UserGroup]] = relationship(foreign_keys=created_by)
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

    # def get_badges(self):
    #     return self


class BadgeGiven(db.Model):
    __tablename__ = "badgegiven"

    # todo: Add nullable=False
    id = db.Column(db.Integer, primary_key=True)
    message = db.Column(db.String)
    # todo: Add nullable=False
    badge_id: Mapped[int] = mapped_column(ForeignKey("badge.id"))
    # todo: Add nullable=False
    group_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"))
    active = db.Column(db.Boolean, nullable=False)

    badge: Mapped[Badge] = relationship()
    group: Mapped[UserGroup] = relationship()

    # def get_group_name(self):
    #     return self.group.name

    # def get_badge_id(self):
    #     return self.badge.id
