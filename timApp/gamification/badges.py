import json

from docutils.nodes import title
from sqlalchemy import ForeignKey
from sqlalchemy.orm import Mapped, mapped_column, relationship

from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class Badge(db.Model):
    __tablename__ = "badge"

    id = db.Column(db.Integer, primary_key=True)
    title = db.Column(db.String, nullable=False)
    description = db.Column(db.String, nullable=False)
    color = db.Column(db.String, nullable=False)
    shape = db.Column(db.String, nullable=False)
    image = db.Column(db.Integer, nullable=False)

    def to_json(self) -> dict:
        return {
            "id": self.id,
            "title": self.title,
            "description": self.description,
            "color": self.color,
            "shape": self.shape,
        }

    def get_badges(self):
        return self


class BadgeGiven(db.Model):
    __tablename__ = "badgegiven"

    id = db.Column(db.Integer, primary_key=True)
    badge_id: Mapped[int] = mapped_column(ForeignKey("badge.id"))
    group_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"))

    badge: Mapped[Badge] = relationship()
    group: Mapped[UserGroup] = relationship()

    def get_group_name(self):
        return self.group.name

    def get_badge_id(self):
        return self.badge.id
