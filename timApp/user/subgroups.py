from __future__ import annotations
from sqlalchemy import ForeignKey
from sqlalchemy.orm import (
    mapped_column,
    relationship,
)

from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class SubGroup(db.Model):
    """
    Implements sub-UserGroups as a relationship between UserGroups.
    Subgroups have the following constraints:
    - a subgroup cannot exist independently,
    - a subgroup cannot have subgroups of its own (recursive subgroups are not allowed),
    - a subgroup must belong to one and only one UserGroup,
    - a subgroup's members must also belong to its parent UserGroup,
    """

    __tablename__ = "usergroup_subgroups"

    parent_id = mapped_column(
        ForeignKey("usergroup.id"),
        primary_key=True,
    )

    child_id = mapped_column(
        ForeignKey("usergroup.id"),
        unique=True,
        nullable=False,
    )

    parent = relationship(
        "UserGroup",
        foreign_keys=[parent_id],
        back_populates="subgroups",
    )

    child = relationship(
        "UserGroup",
        foreign_keys=[child_id],
    )

    @property
    def parent_group(self) -> UserGroup:
        return self.parent
