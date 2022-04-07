"""
Database model for :class:`UserGroupMember` and helper functions to work with it.

In TIM, users can belong to one or multiple user groups (:class:`UserGroup`).
Group membership contains useful information about the user such as:

* When the user was added to the group
* Who added the user to the group
* When the user's membership was expired

All this information is contained in :class:`UserGroupMember` which links a user to the group they belong to.
"""

from sqlalchemy import func

from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


class UserGroupMember(db.Model):
    """
    Associates a user with a user group.
    """

    __tablename__ = "usergroupmember"

    usergroup_id = db.Column(
        db.Integer, db.ForeignKey("usergroup.id"), primary_key=True
    )
    """ID of the usergroup the member belongs to."""

    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
    """ID of the user that belongs to the usergroup."""

    membership_end = db.Column(db.DateTime(timezone=True))
    """Timestamp for when the membership ended.
    
    .. note:: The timestamp is used to determine soft deletion.
              If the end timestamp is present, the user is considered deleted from the group.
    """

    membership_added = db.Column(db.DateTime(timezone=True), default=get_current_time)
    """Timestamp for when the user was last time added as the active member.
    
    .. note:: The timestamp is used **for logging purposes only**.
              In other words, it is not used to determine soft deletion or other membership state.
    """

    added_by = db.Column(db.Integer, db.ForeignKey("useraccount.id"))
    """User ID of the user who added the membership."""

    user = db.relationship(
        "User",
        foreign_keys=[user_id],
    )
    """User that this membership belongs to. Relationship of the :attr:`user_id` column."""

    adder = db.relationship(
        "User",
        foreign_keys=[added_by],
    )
    """User that added this membership. Relationship of the :attr:`added_by` column."""

    group = db.relationship(
        "UserGroup",
    )
    """Group that this membership belongs to. Relationship of the :attr:`usergroup_id` column."""

    def set_expired(self, sync_mailing_lists: bool = True) -> None:
        """
        Expires this membership.

        .. note:: Expired membership is not permanently deleted.
                  Instead, :attr:`membership_end` is set to the current time.

        :param sync_mailing_lists: If True, informs the mailing lists of the change immediately.
        """
        self.membership_end = get_current_time()
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
