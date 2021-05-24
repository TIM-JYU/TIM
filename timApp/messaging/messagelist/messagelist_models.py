from enum import Enum
from typing import List, Optional, Dict, Any

from sqlalchemy.orm.exc import MultipleResultsFound, NoResultFound  # type: ignore

from timApp.messaging.messagelist.listoptions import ArchiveType, Channel, ReplyToListChanges
from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


class MemberJoinMethod(Enum):
    """How a user was added to a message list."""
    DIRECT_ADD = 1
    """The owner of the list has just added this member. The member wasn't asked. This is the only join method that 
    makes sense for groups. """
    INVITED = 2
    """User was invited and they confirmed joining."""
    JOINED = 3
    """User joined the list on their own."""


class MessageListModel(db.Model):
    """Database model for message lists"""

    __tablename__ = "messagelist"

    id = db.Column(db.Integer, primary_key=True)

    manage_doc_id = db.Column(db.Integer, db.ForeignKey("block.id"))
    """The document which manages a message list."""

    name = db.Column(db.Text)
    """The name of a message list."""

    can_unsubscribe = db.Column(db.Boolean)
    """If a member can unsubscribe from this list on their own."""

    email_list_domain = db.Column(db.Text)
    """The domain used for an email list attached to a message list. If None/null, then message list doesn't have an 
    attached email list. This is a tad silly at this point in time, because JYU TIM only has one domain. However, 
    this allows quick adaptation if more domains are added or otherwise changed in the future. """

    archive = db.Column(db.Enum(ArchiveType))
    """The archive policy of a message list."""

    notify_owner_on_change = db.Column(db.Boolean)
    """Should the owner of the message list be notified if there are changes on message list members."""

    description = db.Column(db.Text)
    """A short description what a message list is about."""

    info = db.Column(db.Text)
    """Additional information about the message list."""

    removed = db.Column(db.DateTime(timezone=True))
    """When this list has been marked for removal."""

    default_send_right = db.Column(db.Boolean)
    """Default send right for new members who join the list on their own."""

    default_delivery_right = db.Column(db.Boolean)
    """Default delivery right for new members who join the list on their own."""

    tim_user_can_join = db.Column(db.Boolean)
    """Flag if TIM users can join the list on their own."""

    subject_prefix = db.Column(db.Text)
    """What prefix message subjects that go through the list get."""

    only_text = db.Column(db.Boolean)
    """Flag if only text format messages are allowed on a list."""

    default_reply_type = db.Column(db.Enum(ReplyToListChanges))
    """Default reply type for the list."""

    non_member_message_pass = db.Column(db.Boolean)
    """Flag if non members messages to the list are passed straight through without moderation."""

    allow_attachments = db.Column(db.Boolean)
    """Flag if attachments are allowed on the list. The list of allowed attachment file extensions are stored at 
    listoptions.py """

    block = db.relationship("Block", back_populates="managed_messagelist", lazy="select")
    """Relationship to the document that is used to manage this message list."""

    members = db.relationship("MessageListMember", back_populates="message_list", lazy="select")
    """All the members of the list."""

    distribution = db.relationship("MessageListDistribution", back_populates="message_list", lazy="select")
    """The message channels the list uses."""

    @staticmethod
    def get_list_by_manage_doc_id(doc_id: int) -> 'MessageListModel':
        m = MessageListModel.query.filter_by(manage_doc_id=doc_id).one()
        return m

    @staticmethod
    def get_list_by_name_exactly_one(name: str) -> 'MessageListModel':
        """Get a message list. Use this when the list is expected to exist.

        Raise NotExist exception, if the message list is not found (or technically if multiple ones are found).

        :param name: The name of the list.
        :return: MessageListModel object, if a MessageListModel with attribute name is found.
        """
        try:
            m = MessageListModel.query.filter_by(name=name).one()
            return m
        except (MultipleResultsFound, NoResultFound):
            from timApp.util.flask.requesthelper import NotExist
            raise NotExist(f"No message list named {name}")

    @staticmethod
    def get_list_by_name_first(name_candidate: str) -> 'MessageListModel':
        """Get a message list by it's name, if a list with said name exists.

        :param name_candidate: The name of the message list.
        :return: Return the message list after query by name. Returns at most one result or None if no there are hits.
        """
        m = MessageListModel.query.filter_by(name=name_candidate).first()
        return m

    @staticmethod
    def name_exists(name_candidate: str) -> bool:
        """Check if given name already exists among message lists.

        :param name_candidate: The name we are checking if it already is already in use by another list.
        """
        maybe_list = MessageListModel.get_list_by_name_first(name_candidate=name_candidate)
        if maybe_list is None:
            return False
        else:
            return True

    @property
    def archive_policy(self) -> ArchiveType:
        return self.archive

    def get_individual_members(self) -> List['MessageListMember']:
        """Get all the members that are not groups."""
        individuals = []
        for member in self.members:
            if not member.is_group():
                individuals.append(member)
        return individuals

    def get_tim_members(self) -> List['MessageListTimMember']:
        """Get all members that have belong to a user group, i.e. TIM users and user groups.

        :return: A list of MessageListTimMember objects.
        """
        tim_members = []
        for member in self.members:
            if member.is_tim_member():
                tim_members.append(member.tim_member)
        return tim_members

    def get_member_by_name(self, name: Optional[str], email: Optional[str]) -> Optional['MessageListMember']:
        """Get member of this list. Member can be searched with name and/or email. At least one has to be given. Name
        is preferred and is used in a search first.

        Raises ValueError if used with both name and email parameters as None.

        :param name: Name
        :param email: Email address
        :return: A message list member, if one is found with given arguments.
        """
        if not name and not email:
            raise ValueError

        for member in self.members:
            if name is not None and name == member.get_name():
                return member
            if email is not None and email == member.get_email():
                return member
        return None


class MessageListMember(db.Model):
    """Database model for members of a message list."""

    __tablename__ = "messagelist_member"

    id = db.Column(db.Integer, primary_key=True)

    message_list_id = db.Column(db.Integer, db.ForeignKey("messagelist.id"))
    """What message list a member belongs to."""

    send_right = db.Column(db.Boolean)
    """If a member can send messages to a message list. Send right for a user group is meaningless at this point"""

    delivery_right = db.Column(db.Boolean)
    """If a member can get messages from a message list. Delivery right for a user group is meaningless at this 
    point. """

    membership_ended = db.Column(db.DateTime(timezone=True))
    """When member's membership on a list ended. This is set when member is removed from a list. A value of None means 
    the member is still on the list."""

    join_method = db.Column(db.Enum(MemberJoinMethod))
    """How the member came to a list."""

    membership_verified = db.Column(db.DateTime(timezone=True))
    """When the user's joining was verified. If user is added e.g. by a teacher to a course's message list, 
    this date is the date teacher added the member. If the member was invited, then this is the date they verified 
    their join. """

    member_type = db.Column(db.Text)
    """Discriminator for polymorhphic members."""

    message_list = db.relationship("MessageListModel", back_populates="members", lazy="select", uselist=False)
    tim_member = db.relationship("MessageListTimMember", back_populates="member", lazy="select",
                                 uselist=False, post_update=True)
    external_member = db.relationship("MessageListExternalMember", back_populates="member", lazy="select",
                                      uselist=False)
    distribution = db.relationship("MessageListDistribution", back_populates="member", lazy="select")

    __mapper_args__ = {"polymorphic_identity": "member", "polymorphic_on": member_type}

    def is_external_member(self) -> bool:
        """If this member is an external member to a message list."""
        if self.external_member:
            return True
        return False

    def is_tim_member(self) -> bool:
        """If this member is a 'TIM member', i.e. a user group. This can be either a personal user group or a
        group. """
        if self.tim_member:
            return True
        return False

    def is_personal_user(self) -> bool:
        """If this member is an individual user, i.e. a personal user group."""
        try:
            gid = self.tim_member.group_id
        except AttributeError:
            # External members don't have a group_id attribute.
            return self.is_external_member()
        from timApp.user.usergroup import UserGroup
        ug = UserGroup.query.filter_by(id=gid).one()
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
        """If the member is verified to be on the list. """
        return self.membership_verified is not None

    def remove(self) -> None:
        """Shorthand for removing a member out of the group, by setting the membership_ended attribute."""
        self.membership_ended = get_current_time()  # datetime.now()

    def get_email(self) -> str:
        """The process of obtaining member's address varies depending on if the member
        is a TIM user or not. Child classes have their own implementation depending on how they obtain the
        information. This is mostly for helping with types.

        :return: This particular instance raises NotImplementedError. The supposed return value is the user's email
        address.
        """
        raise NotImplementedError


def get_members_for_list(msg_list: MessageListModel) -> List[MessageListMember]:
    """Get all members belonging to a list.

    :param msg_list:
    :return:
    """
    list_members = MessageListMember.query.filter_by(message_list_id=msg_list.id).all()
    return list_members


class MessageListTimMember(MessageListMember):
    """A member of message list who is also a TIM user(group). This can be one person in their own personal user
    group or this can be e.g. a course's group."""

    __tablename__ = "messagelist_tim_member"

    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)

    group_id = db.Column(db.Integer, db.ForeignKey("usergroup.id"))
    """A UserGroup id for a member."""

    member = db.relationship("MessageListMember", back_populates="tim_member", lazy="select",
                             uselist=False, post_update=True)

    user_group = db.relationship("UserGroup", back_populates="messagelist_membership", lazy="select", uselist=False,
                                 post_update=True)

    __mapper_args__ = {"polymorphic_identity": "tim_member"}

    def to_json(self) -> Dict[str, Any]:
        return {
            "name": self.get_name(),
            "email": self.get_email() if self.get_email() is not None else "",
            "sendRight": self.member.send_right,
            "deliveryRight": self.member.delivery_right,
            "removed": self.membership_ended
        }

    def get_name(self) -> str:
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
        return user.email


class MessageListExternalMember(MessageListMember):
    """A member of message list who is *not* a TIM user. Mainly intended for, but not necessary limited to,
    email-list only usage."""

    __tablename__ = "messagelist_external_member"

    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)

    email_address = db.Column(db.Text)
    """Email address of message list's external member."""

    display_name = db.Column(db.Text)

    member = db.relationship("MessageListMember", back_populates="external_member", lazy="select", uselist=False)

    __mapper_args__ = {"polymorphic_identity": "external_member"}

    def to_json(self) -> Dict[str, Any]:
        return {
            "name": self.get_name(),
            "email": self.email_address,
            "sendRight": self.member.send_right,
            "deliveryRight": self.member.delivery_right,
            "removed": self.membership_ended
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


class MessageListDistribution(db.Model):
    """Message list member's chosen distribution channels."""

    __tablename__ = "messagelist_distribution"

    id = db.Column(db.Integer, primary_key=True)

    user_id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"))
    """Message list member's id, if this row is about message list member's channel distribution."""

    message_list_id = db.Column(db.Integer, db.ForeignKey("messagelist.id"))
    """Message list's id, if this row is about message list's channel distribution."""

    channel = db.Column(db.Enum(Channel))
    """Which message channels are used for a message list."""

    member = db.relationship("MessageListMember", back_populates="distribution", lazy="select", uselist=False)
    message_list = db.relationship("MessageListModel", back_populates="distribution", lazy="select", uselist=False)


class UserEmails(db.Model):
    """TIM users' additional emails."""

    __tablename__ = "user_emails"

    id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)

    additional_email = db.Column(db.Text, unique=True)
    """The users another email."""

    is_primary_email = db.Column(db.Boolean)
    """Which one of the additional emails is user's primary email address. If none are, the email address in users 
    own table is assumed to be primary email address. """

    address_verified = db.Column(db.DateTime(timezone=True))
    """The user has to verify they are in the possession of the email address."""


class VerificationType(Enum):
    """Type of verification, used to direct the proper verification action afterwards."""
    LIST_JOIN = 1
    EMAIL_OWNERSHIP = 2


class Verifications(db.Model):
    """For various pending verifications, such as message list joining and email ownership verification."""
    __tablename__ = "verifications"

    id = db.Column(db.Integer, primary_key=True)

    verification_type = db.Column(db.Enum(VerificationType))
    """The type of verification, see VerificationType class for details."""

    verification_pending = db.Column(db.DateTime(timezone=True))
    """When a verification has been added to db, pending sending to a user."""

    verification_link = db.Column(db.Text)
    """Generated verification link. This is given to the user and once they click on it, they are verified (in 
    whatever it was that needed verification)."""

    verified = db.Column(db.DateTime(timezone=True))
    """When the user used the link to verify."""
