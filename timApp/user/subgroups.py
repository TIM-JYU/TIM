from __future__ import annotations

from typing import TYPE_CHECKING

from sqlalchemy import CheckConstraint, ForeignKey
from sqlalchemy.orm import (
    Mapped,
    mapped_column,
    relationship,
)

from timApp.timdb.sqa import db
from timApp.user.special_group_names import PRIVILEGED_GROUPS, SPECIAL_GROUPS

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.user.usergroup import UserGroup


class SubGroupError(Exception):
    """Raised when an operation would break one of the subgroup constraints.

    Route handlers should catch this and turn it into a RouteException.
    """


class SubGroup(db.Model):
    """
    Implements sub-UserGroups as a relationship between UserGroups.
    Subgroups have the following constraints:
    - a subgroup cannot exist independently,
    - a subgroup cannot have subgroups of its own (recursive subgroups are not allowed),
    - a subgroup must belong to one and only one UserGroup,
    - a subgroup's members must also belong to its parent UserGroup,

    A subgroup is an ordinary UserGroup in every other respect. The hierarchy is
    therefore always at most one level deep: a UserGroup is either a parent, with any
    number of subgroups, or a subgroup with exactly one parent, but never both.

    ``child_id`` is the primary key, which is what enforces "one parent only" in the
    database. The no-chaining rule spans rows and cannot be expressed as a row-local
    SQL constraint, so it is enforced by :func:`add_subgroup`.

    Membership is materialised rather than derived: a member of a subgroup is also a
    real member of the parent group, so reading a group's members never has to walk
    this table. :func:`add_subgroup` back-fills the parent when the link is created,
    and :meth:`timApp.user.user.User.add_to_group` keeps it up to date afterwards.
    """

    __tablename__ = "usergroup_subgroups"

    __table_args__ = (
        CheckConstraint(
            "parent_id <> child_id",
            name="usergroup_subgroups_no_self_reference",
        ),
    )

    child_id: Mapped[int] = mapped_column(
        ForeignKey("usergroup.id"),
        primary_key=True,
    )
    """The subgroup. Primary key, so a group can be a subgroup of at most one parent."""

    parent_id: Mapped[int] = mapped_column(
        ForeignKey("usergroup.id"),
        nullable=False,
        index=True,
    )
    """The UserGroup that the subgroup belongs to."""

    parent: Mapped["UserGroup"] = relationship(
        foreign_keys=[parent_id],
        back_populates="subgroups",
    )

    child: Mapped["UserGroup"] = relationship(
        foreign_keys=[child_id],
        back_populates="subgroup_of",
    )

    def __repr__(self) -> str:
        return f"<SubGroup(parent_id={self.parent_id}, child_id={self.child_id})>"


def _check_eligible(ug: UserGroup, role: str) -> None:
    """Reject groups that cannot take part in a subgroup relationship at all.

    Privileged groups are excluded because membership is materialised: linking
    e.g. the teachers group would make every teacher a real member of the parent.
    """
    if ug.name in SPECIAL_GROUPS:
        raise SubGroupError(f"Special group '{ug.name}' cannot be {role}.")
    if ug.name in PRIVILEGED_GROUPS:
        raise SubGroupError(f"Privileged group '{ug.name}' cannot be {role}.")
    if ug.is_personal_group:
        raise SubGroupError(f"Personal group '{ug.name}' cannot be {role}.")


def add_subgroup(
    parent: UserGroup, child: UserGroup, added_by: User | None = None
) -> SubGroup:
    """Make ``child`` a subgroup of ``parent``.

    Members of ``child`` are added to ``parent`` as well, because a subgroup's members
    are implicitly members of its parent. Re-adding an existing pairing is a no-op that
    returns the existing link.

    :param parent: The group that gains a subgroup.
    :param child: The group that becomes a subgroup.
    :param added_by: The user credited with the resulting parent memberships.
    :raises SubGroupError: if the pairing would break any subgroup constraint.
    """
    if parent is child or (parent.id is not None and parent.id == child.id):
        raise SubGroupError(f"Group '{parent.name}' cannot be a subgroup of itself.")

    _check_eligible(parent, "a parent group")
    _check_eligible(child, "a subgroup")

    existing = child.subgroup_of
    if existing is not None:
        if existing.parent_id == parent.id:
            return existing
        raise SubGroupError(
            f"Group '{child.name}' is already a subgroup of '{existing.parent.name}'. "
            f"A subgroup can belong to only one group."
        )

    # Both directions of the no-chaining rule.
    if parent.is_subgroup:
        raise SubGroupError(
            f"Group '{parent.name}' is itself a subgroup of "
            f"'{parent.parent_group.name}', so it cannot have subgroups of its own."
        )
    if child.subgroups:
        raise SubGroupError(
            f"Group '{child.name}' has subgroups of its own, so it cannot become a "
            f"subgroup. Remove its subgroups first."
        )

    link = SubGroup(parent=parent, child=child)
    db.session.add(link)

    # Materialise the implicit memberships. add_to_group is a no-op for users who are
    # already members of the parent, so direct and inherited membership can overlap.
    for member in child.users:
        member.add_to_group(parent, added_by)

    return link


def remove_subgroup(parent: UserGroup, child: UserGroup) -> None:
    """Detach ``child`` from ``parent``, leaving both as ordinary UserGroups.

    :raises SubGroupError: if ``child`` is not a subgroup of ``parent``.
    """
    link = child.subgroup_of
    if link is None or link.parent_id != parent.id:
        raise SubGroupError(
            f"Group '{child.name}' is not a subgroup of '{parent.name}'."
        )
    db.session.delete(link)
