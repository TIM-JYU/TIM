"""
Database model for :class:`UserGroupMember` and helper functions to work with it.

In TIM, users can belong to one or multiple user groups (:class:`UserGroup`).
Group membership contains useful information about the user such as:

* When the user was added to the group
* Who added the user to the group
* When the user's membership was expired

All this information is contained in :class:`UserGroupMember` which links a user to the group they belong to.
"""
from datetime import timedelta
from typing import Optional, TYPE_CHECKING

from sqlalchemy import func, ForeignKey, select
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.user.subgroups import SubGroup
from timApp.util.utils import get_current_time

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.user.usergroup import UserGroup


class UserGroupMember(db.Model):
    """
    Associates a user with a user group.
    """

    usergroup_id: Mapped[int] = mapped_column(
        ForeignKey("usergroup.id"), primary_key=True
    )
    """ID of the usergroup the member belongs to."""

    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), primary_key=True)
    """ID of the user that belongs to the usergroup."""

    membership_end: Mapped[Optional[datetime_tz]]
    """Timestamp for when the membership ended.
    
    .. note:: The timestamp is used to determine soft deletion.
              If the end timestamp is present, the user is considered deleted from the group.
    """

    membership_added: Mapped[Optional[datetime_tz]] = mapped_column(
        default=get_current_time
    )
    """Timestamp for when the user was last time added as the active member.
    
    .. note:: The timestamp is used **for logging purposes only**.
              In other words, it is not used to determine soft deletion or other membership state.
    """

    added_by: Mapped[Optional[int]] = mapped_column(ForeignKey("useraccount.id"))
    """User ID of the user who added the membership."""

    user: Mapped["User"] = relationship(foreign_keys=[user_id])
    """User that this membership belongs to. Relationship of the :attr:`user_id` column."""

    adder: Mapped[Optional["User"]] = relationship(foreign_keys=[added_by])
    """User that added this membership. Relationship of the :attr:`added_by` column."""

    group: Mapped["UserGroup"] = relationship()
    """Group that this membership belongs to. Relationship of the :attr:`usergroup_id` column."""

    def get_subgroup_memberships(self) -> list["UserGroupMember"]:
        """This user's active memberships in the subgroups of this membership's group.

        Resolved by joining the ``usergroup_subgroups`` table, so nothing has to be
        recorded on the membership itself to know which ones were inherited.
        """
        return list(
            run_sql(
                select(UserGroupMember)
                .join(SubGroup, SubGroup.child_id == UserGroupMember.usergroup_id)
                .where(
                    (SubGroup.parent_id == self.usergroup_id)
                    & (UserGroupMember.user_id == self.user_id)
                    & membership_current
                )
            )
            .scalars()
            .all()
        )

    def set_expired(
        self,
        time_offset: timedelta | None = None,
        sync_mailing_lists: bool = True,
        propagate_to_subgroups: bool = True,
    ) -> None:
        """
        Expires this membership.

        .. note:: Expired membership is not permanently deleted.
                  Instead, :attr:`membership_end` is set to the current time.

        Removing a member from a group also removes them from that group's subgroups,
        because a subgroup's members are always members of its parent. The propagation
        is one-way: leaving a subgroup never touches the parent membership, since a
        user may belong to the parent without belonging to any of its subgroups.

        :param time_offset: The offset to the expiration date.
        :param sync_mailing_lists: If True, informs the mailing lists of the change immediately.
        :param propagate_to_subgroups: If True, expires this user's memberships in the
               subgroups of this group as well.
        """
        delta = time_offset if time_offset else timedelta(seconds=0)
        now = get_current_time()
        # An already-expired membership has already propagated; re-expiring it must not
        # expire subgroup memberships that were granted again since.
        was_active = self.membership_end is None or self.membership_end > now
        self.membership_end = now - delta
        if sync_mailing_lists:
            from timApp.messaging.messagelist.messagelist_utils import (
                sync_message_list_on_expire,
            )

            sync_message_list_on_expire(self.user, self.group)

        if propagate_to_subgroups and was_active:
            for membership in self.get_subgroup_memberships():
                # Subgroups cannot be nested, so there is no level below this one.
                membership.set_expired(
                    time_offset,
                    sync_mailing_lists=sync_mailing_lists,
                    propagate_to_subgroups=False,
                )


membership_current = (UserGroupMember.membership_end == None) | (
    func.current_timestamp() < UserGroupMember.membership_end
)
"""SQLAlchemy query selector which selects active memberships."""

membership_deleted = func.current_timestamp() >= UserGroupMember.membership_end
"""SQLAlchemy query selector which selects expired memberships."""
