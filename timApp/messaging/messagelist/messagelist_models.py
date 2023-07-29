from datetime import datetime
from enum import Enum
from typing import Optional, Any, TYPE_CHECKING, List

from sqlalchemy import select, ForeignKey
from sqlalchemy.ext.hybrid import hybrid_property  # type: ignore
from sqlalchemy.orm import mapped_column, Mapped, relationship
from sqlalchemy.orm.exc import MultipleResultsFound, NoResultFound  # type: ignore

from timApp.messaging.messagelist.listinfo import (
    ArchiveType,
    Channel,
    ReplyToListChanges,
    ListInfo,
    Distribution,
    MessageVerificationType,
)
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz, DbModel
from timApp.util.utils import get_current_time

if TYPE_CHECKING:
    from timApp.item.block import Block
    from timApp.user.usergroup import UserGroup


class MemberJoinMethod(Enum):
    """How a user was added to a message list."""

    DIRECT_ADD = 1
    """The owner of the list has just added this member. The member wasn't asked. This is the only join method that 
    makes sense for groups. """
    INVITED = 2
    """User was invited and they confirmed joining."""
    JOINED = 3
    """User joined the list on their own."""


class MessageListModel(DbModel):
    """Database model for message lists"""

    __tablename__ = "messagelist"

    id: Mapped[int] = mapped_column(primary_key=True)

    manage_doc_id: Mapped[Optional[int]] = mapped_column(ForeignKey("block.id"))
    """The document which manages a message list."""

    name: Mapped[Optional[str]]
    """The name of a message list."""

    can_unsubscribe: Mapped[Optional[bool]]
    """If a member can unsubscribe from this list on their own."""

    email_list_domain: Mapped[Optional[str]]
    """The domain used for an email list attached to a message list. If None/null, then message list doesn't have an 
    attached email list. This is a tad silly at this point in time, because JYU TIM only has one domain. However, 
    this allows quick adaptation if more domains are added or otherwise changed in the future. """

    archive: Mapped[Optional[ArchiveType]]
    """The archive policy of a message list."""

    notify_owner_on_change: Mapped[Optional[bool]]
    """Should the owner of the message list be notified if there are changes on message list members."""

    description: Mapped[Optional[str]]
    """A short description what a message list is about."""

    info: Mapped[Optional[str]]
    """Additional information about the message list."""

    removed: Mapped[Optional[datetime_tz]]
    """When this list has been marked for removal."""

    default_send_right: Mapped[Optional[bool]]
    """Default send right for new members who join the list on their own."""

    default_delivery_right: Mapped[Optional[bool]]
    """Default delivery right for new members who join the list on their own."""

    tim_user_can_join: Mapped[Optional[bool]]
    """Flag if TIM users can join the list on their own."""

    subject_prefix: Mapped[Optional[str]]
    """What prefix message subjects that go through the list get."""

    only_text: Mapped[Optional[bool]]
    """Flag if only text format messages are allowed on a list."""

    default_reply_type: Mapped[Optional[ReplyToListChanges]]
    """Default reply type for the list."""

    non_member_message_pass: Mapped[Optional[bool]]
    """Flag if non members messages to the list are passed straight through without moderation."""

    allow_attachments: Mapped[Optional[bool]]
    """Flag if attachments are allowed on the list. The list of allowed attachment file extensions are stored at 
    listoptions.py """

    message_verification: Mapped[MessageVerificationType] = mapped_column(
        default=MessageVerificationType.MUNGE_FROM,
    )
    """How to verify messages sent to the list."""

    block: Mapped["Block"] = relationship(
        back_populates="managed_messagelist", lazy="select"
    )
    """Relationship to the document that is used to manage this message list."""

    members: Mapped[List["MessageListMember"]] = relationship(
        back_populates="message_list", lazy="select"
    )
    """All the members of the list."""

    distribution: Mapped["MessageListDistribution"] = relationship(
        back_populates="message_list", lazy="select"
    )
    """The message channels the list uses."""

    @staticmethod
    def get_by_email(email: str) -> Optional["MessageListModel"]:
        name, domain = email.split("@", 1)
        return (
            run_sql(
                select(MessageListModel).filter_by(name=name, email_list_domain=domain)
            )
            .scalars()
            .first()
        )

    @staticmethod
    def from_manage_doc_id(doc_id: int) -> "MessageListModel":
        return (
            run_sql(select(MessageListModel).filter_by(manage_doc_id=doc_id))
            .scalars()
            .one()
        )

    @staticmethod
    def from_name(name: str) -> "MessageListModel":
        """Gets a message list from name or throws a NotExist error if no unique list exists.

        :param name: The name of the list.
        :return: MessageListModel object, if a MessageListModel with attribute name is found.
        """
        from timApp.util.flask.requesthelper import NotExist

        m = MessageListModel.get_by_name(name)
        if not m:
            raise NotExist(f"No message list named {name}")
        return m

    @staticmethod
    def get_by_name(name_candidate: str) -> Optional["MessageListModel"]:
        """Get a message list by its name, if a list with said name exists.

        :param name_candidate: The name of the message list.
        :return: Return the message list after query by name. Returns at most one result or None if no there are hits.
        """
        return (
            run_sql(select(MessageListModel).filter_by(name=name_candidate))
            .scalars()
            .first()
        )

    @staticmethod
    def name_exists(name_candidate: str) -> bool:
        """Check if given name already exists among message lists.

        :param name_candidate: The name we are checking if it already is already in use by another list.
        """
        return (
            run_sql(
                select(MessageListModel.name).filter_by(name=name_candidate).limit(1)
            )
            .scalars()
            .first()
            is not None
        )

    @property
    def archive_policy(self) -> ArchiveType:
        return self.archive

    @hybrid_property
    def mailman_list_id(self) -> str:
        return self.name + "." + self.email_list_domain

    def get_individual_members(self) -> list["MessageListMember"]:
        """Get all the members that are not groups.

        :return: A list of message list's members, who are individual TIM users (MessageListTimMember objects) or
        external members (MessageListExternalMember objects).
        """
        individuals = []
        for member in self.members:
            if not member.is_group():
                individuals.append(member)
        return individuals

    def get_tim_members(self) -> list["MessageListTimMember"]:
        """Get all members that have belong to a user group, i.e. TIM users and user groups.

        :return: A list of MessageListTimMember objects.
        """
        tim_members = []
        for member in self.members:
            if member.is_tim_member():
                tim_members.append(member.tim_member)
        return tim_members

    def find_member(
        self, username: str | None, email: str | None
    ) -> Optional["MessageListMember"]:
        """Get member of this list. Member can be searched with username and/or email. At least one has to be given. If
        both are given, username is preferred and is used in a search first.

        Raises ValueError if used with both name and email parameters as None.

        :param username: Userame of the member.
        :param email: Member's email address
        :return: A message list member, if one is found with given arguments. Otherwise return None.
        """
        if not username and not email:
            raise ValueError

        for member in self.members:
            if username and username == member.get_username():
                return member
            if email and email == member.get_email():
                return member
        return None

    @property
    def email_address(self) -> str | None:
        """Full email address of the messagelist, if the list has been assigned an active address.
        Otherwise None."""
        return (
            f"{self.name}@{self.email_list_domain}" if self.email_list_domain else None
        )

    def to_info(self) -> ListInfo:
        from timApp.messaging.messagelist.emaillist import get_list_ui_link

        return ListInfo(
            name=self.name,
            notify_owners_on_list_change=self.notify_owner_on_change,
            domain=self.email_list_domain,
            archive=self.archive,
            default_reply_type=self.default_reply_type,
            verification_type=self.message_verification,
            tim_users_can_join=self.tim_user_can_join,
            list_subject_prefix=self.subject_prefix,
            members_can_unsubscribe=self.can_unsubscribe,
            default_send_right=self.default_send_right,
            default_delivery_right=self.default_delivery_right,
            only_text=self.only_text,
            non_member_message_pass=self.non_member_message_pass,
            email_admin_url=get_list_ui_link(self.name, self.email_list_domain),
            list_info=self.info,
            list_description=self.description,
            allow_attachments=self.allow_attachments,
            distribution=Distribution(email_list=True, tim_message=True),
            removed=self.removed,
        )


class MessageListMember(DbModel):
    """Database model for members of a message list."""

    __tablename__ = "messagelist_member"

    id: Mapped[int] = mapped_column(primary_key=True)

    message_list_id: Mapped[Optional[int]] = mapped_column(ForeignKey("messagelist.id"))
    """What message list a member belongs to."""

    send_right: Mapped[Optional[bool]]
    """If a member can send messages to a message list."""

    delivery_right: Mapped[Optional[bool]]
    """If a member can get messages from a message list."""

    membership_ended: Mapped[Optional[datetime_tz]]
    """When member's membership on a list ended. This is set when member is removed from a list. A value of None means 
    the member is still on the list."""

    join_method: Mapped[Optional[MemberJoinMethod]]
    """How the member came to a list."""

    membership_verified: Mapped[Optional[datetime_tz]]
    """When the user's joining was verified. If user is added e.g. by a teacher to a course's message list, 
    this date is the date teacher added the member. If the member was invited, then this is the date they verified 
    their join. """

    member_type: Mapped[Optional[str]]
    """Discriminator for polymorhphic members."""

    message_list: Mapped["MessageListModel"] = relationship(
        back_populates="members", lazy="select"
    )
    tim_member: Mapped[Optional["MessageListTimMember"]] = relationship(
        back_populates="member",
        lazy="select",
        post_update=True,
    )
    external_member: Mapped[Optional["MessageListExternalMember"]] = relationship(
        back_populates="member",
        lazy="select",
        uselist=False,
        post_update=True,
    )
    distribution: Mapped[Optional["MessageListDistribution"]] = relationship(
        back_populates="member", lazy="select"
    )

    __mapper_args__ = {
        "polymorphic_identity": "member",
        "polymorphic_on": "member_type",
    }

    def is_external_member(self) -> bool:
        """If this member is an external member to a message list."""
        if self.external_member:
            return True
        return False

    def is_tim_member(self) -> bool:
        """If this member is a 'TIM member', i.e. a user group. This can be either a personal user group or a
        group."""
        if self.tim_member:
            return True
        return False

    def is_personal_user(self) -> bool:
        """If this member is an individual user, i.e. a personal user group or an external member."""
        try:
            gid = self.tim_member.group_id
        except AttributeError:
            # External members don't have a group_id attribute.
            return self.is_external_member()
        from timApp.user.usergroup import UserGroup

        ug = db.session.exectute(select(UserGroup).filter_by(id=gid)).scalars().one()
        return ug.is_personal_group

    def is_group(self) -> bool:
        """If this message list member is actually a group of users."""
        return not self.is_personal_user()

    def is_active(self) -> bool:
        """Check if the message list's member is an active member of the list. A member is an active member if they have
         been verified and have not been removed from the list.

        :return: True if the member is both verified and not removed. Otherwise returns False.
        """
        return self.membership_ended is None and self.is_verified()

    def is_verified(self) -> bool:
        """If the member is verified to be on the list."""
        return self.membership_verified is not None

    def remove(self, end_time: datetime | None = get_current_time()) -> None:
        """Shorthand for removing a member out of the group, by setting the membership_ended attribute."""
        self.membership_ended = end_time

    def get_email(self) -> str:
        """The process of obtaining member's address varies depending on if the member
        is a TIM user or not. Child classes have their own implementation depending on how they obtain the
        information. This is mostly for helping with types.

        :return: This particular instance raises NotImplementedError. The supposed return value is the user's email
        address.
        """
        raise NotImplementedError

    def user_info_json(self) -> dict[str, Any]:
        raise NotImplementedError

    def to_json(self) -> dict[str, Any]:
        from timApp.document.hide_names import is_hide_names

        if is_hide_names():
            is_group = self.is_group()
            user_info = {
                "name": f"Member {self.id} ({ 'Group' if is_group else 'User' })",
                "username": f"member{self.id}",
                "email": "" if is_group else "member@noreply",
            }
        else:
            user_info = self.user_info_json()

        return {
            **user_info,
            "sendRight": self.member.send_right,
            "deliveryRight": self.member.delivery_right,
            "removed": self.membership_ended,
        }


class MessageListTimMember(MessageListMember):
    """A member of message list who is also a TIM user(group). This can be one person in their own personal user
    group or this can be e.g. a course's group."""

    __tablename__ = "messagelist_tim_member"

    id: Mapped[int] = mapped_column(
        ForeignKey("messagelist_member.id"), primary_key=True
    )

    group_id: Mapped[Optional[int]] = mapped_column(ForeignKey("usergroup.id"))
    """A UserGroup id for a member."""

    member: Mapped["MessageListMember"] = relationship(
        back_populates="tim_member",
        post_update=True,
    )

    user_group: Mapped["UserGroup"] = relationship(
        back_populates="messagelist_membership",
        post_update=True,
    )

    __mapper_args__ = {"polymorphic_identity": "tim_member"}

    def user_info_json(self) -> dict[str, Any]:
        return {
            "name": self.get_name(),
            "username": self.get_username(),
            "email": self.get_email(),
        }

    def get_username(self) -> str:
        """Get the TIM user group's name."""
        ug = self.user_group
        return ug.name

    def get_email(self) -> str:
        """Get TIM user group's email. Email makes sense only for personal user groups. Using this method for groups
        returns an empty string"""
        if self.is_group():
            return ""
        ug = self.user_group
        user = ug.personal_user
        return user.email or ""

    def get_name(self) -> str:
        """Get TIM user's name. For group, this is an empty string. For a user, this is their full name."""
        if not self.is_group():
            ug = self.user_group
            user = ug.personal_user
            return user.pretty_full_name
        return ""


class MessageListExternalMember(MessageListMember):
    """A member of message list who is *not* a TIM user. Mainly intended for, but not necessary limited to,
    email-list only usage."""

    __tablename__ = "messagelist_external_member"

    id: Mapped[int] = mapped_column(
        ForeignKey("messagelist_member.id"), primary_key=True
    )

    email_address: Mapped[Optional[str]]
    """Email address of message list's external member."""

    display_name: Mapped[Optional[str]]
    """Display name for external user, which in most cases should be the external member's address' owner's name."""

    member: Mapped["MessageListMember"] = relationship(back_populates="external_member")

    __mapper_args__ = {"polymorphic_identity": "external_member"}

    def user_info_json(self) -> dict[str, Any]:
        return {
            "name": self.get_name(),
            "username": self.get_username(),
            "email": self.email_address,
        }

    def get_name(self) -> str:
        """Get the external member's name, if one has been specified.

        :return:The display name or an empty string.
        """
        return self.display_name if self.display_name is not None else ""

    def get_email(self) -> str:
        """Get message list's external member's email.

        :return: The email address.
        """
        return self.email_address

    def get_username(self) -> str:
        """External member's don't have usernames, but this is for consistency when using other methods."""
        return ""


class MessageListDistribution(DbModel):
    """Message list member's chosen distribution channels."""

    __tablename__ = "messagelist_distribution"

    id: Mapped[int] = mapped_column(primary_key=True)

    user_id: Mapped[Optional[int]] = mapped_column(ForeignKey("messagelist_member.id"))
    """Message list member's id, if this row is about message list member's channel distribution."""

    message_list_id: Mapped[Optional[int]] = mapped_column(ForeignKey("messagelist.id"))
    """Message list's id, if this row is about message list's channel distribution."""

    channel: Mapped[Optional[Channel]]
    """Which message channels are used by a message list or a user."""

    member: Mapped[Optional["MessageListMember"]] = relationship(
        back_populates="distribution", lazy="select"
    )
    message_list: Mapped[Optional["MessageListModel"]] = relationship(
        back_populates="distribution", lazy="select"
    )
