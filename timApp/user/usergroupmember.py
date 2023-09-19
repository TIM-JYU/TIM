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

from sqlalchemy import func, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz
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

    def set_expired(
        self, time_offset: timedelta | None = None, sync_mailing_lists: bool = True
    ) -> None:
        """
        Expires this membership.

        .. note:: Expired membership is not permanently deleted.
                  Instead, :attr:`membership_end` is set to the current time.

        :param time_offset: The offset to the expiration date.
        :param sync_mailing_lists: If True, informs the mailing lists of the change immediately.
        """
        delta = time_offset if time_offset else timedelta(seconds=0)
        self.membership_end = get_current_time() - delta
        if sync_mailing_lists:
            from timApp.messaging.messagelist.messagelist_utils import (
                sync_message_list_on_expire,
            )

            sync_message_list_on_expire(self.user, self.group)


membership_current = (UserGroupMember.membership_end == None) | (
    func.current_timestamp() < UserGroupMember.membership_end
)
"""SQLAlchemy query selector which selects active memberships."""

membership_deleted = func.current_timestamp() >= UserGroupMember.membership_end
"""SQLAlchemy query selector which selects expired memberships."""
