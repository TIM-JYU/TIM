import json
from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy.orm import Mapped, mapped_column, relationship

from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class Badge(db.Model):
    __tablename__ = "badge"

    id = db.Column(db.Integer, primary_key=True, nullable=False)
    title = db.Column(db.String, nullable=False)
    description = db.Column(db.String, nullable=False)
    color = db.Column(db.String, nullable=False)
    shape = db.Column(db.String, nullable=False)
    image = db.Column(db.Integer, nullable=False)
    context_group = db.Column(db.String, nullable=False)
    active = db.Column(db.Boolean, nullable=False)
    # todo: useraccount or usergroup?
    created_by: Mapped[Optional[int]] = mapped_column(
        ForeignKey("usergroup.id"), nullable=False
    )
    created = db.Column(db.DateTime, nullable=False)
    # todo: useraccount or usergroup?
    modified_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    modified = db.Column(db.DateTime)
    # todo: useraccount or usergroup?
    deleted_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    deleted = db.Column(db.DateTime)
    # todo: useraccount or usergroup?
    restored_by: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    restored = db.Column(db.DateTime)

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
    # todo: useraccount or usergroup?
    # todo: Add nullable=False
    group_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"))
    active = db.Column(db.Boolean, nullable=False)

    badge: Mapped[Badge] = relationship()
    # todo: useraccount or usergroup?
    group: Mapped[UserGroup] = relationship()

    # def get_group_name(self):
    #     return self.group.name

    # def get_badge_id(self):
    #     return self.badge.id
