from enum import Enum

from timApp.messaging.messagelist.listoptions import ArchiveType
from timApp.timdb.sqa import db


class Channel(Enum):
    """The message channels TIM uses and provides for message lists."""
    TIM_MESSAGE = 'tim_message'
    EMAIL_LIST = 'email_list'
    # EMAIL = 3


class MessageListJoinMethod(Enum):
    """How a user was added to a message list."""
    DIRECT_ADD = 1
    """The owner of the list has just added this member. The member wasn't asked."""
    INVITED = 2
    """User was invited and they confirmed joining."""


# VIESTIM: The dabase models for message lists. Primarily follow the database plan, if you should deviate from the plan
#  document it so after the project it's easier to update the plan for maintainers.

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

    # VIESTIM: archive is type bool in the original plan.
    archive = db.Column(db.Enum(ArchiveType))
    """The archive policy of a message list."""

    # VIESTIM: New values.

    notify_owner_on_change = db.Column(db.Boolean)
    """Should the owner of the message list be notified if there are changes on message list members."""

    description = db.Column(db.Text)
    """A short description what a message list is about."""

    info = db.Column(db.Text)
    """Additional information about the message list."""

    removed = db.Column(db.DateTime(timezone=True))
    """When this list has been marked for removal."""

    block = db.relationship("Block")
    members = db.relationship("MessageListMember", back_populates="message_list")

    @staticmethod
    def get_list_by_manage_doc_id(doc_id: int) -> 'MessageListModel':
        m = MessageListModel.query.filter_by(manage_doc_id=doc_id).one()
        return m

    @staticmethod
    def get_list_by_name(name: str) -> 'MessageListModel':
        m = MessageListModel.query.filter_by(name=name).one()
        return m


class MessageListMember(db.Model):
    """Database model for members of a message list."""

    __tablename__ = "messagelist_member"

    id = db.Column(db.Integer, primary_key=True)

    message_list_id = db.Column(db.Integer, db.ForeignKey("messagelist.id"))
    """What message list a member belongs to."""

    # VIESTIM: This is can_send in the original database plan.
    send_right = db.Column(db.Boolean)
    """If a member can send messages to a message list."""

    # VIESTIM: delivery_right doesn't exist in the original plan.
    delivery_right = db.Column(db.Boolean)
    """If a member can get messages from a message list."""

    membership_ended = db.Column(db.DateTime(timezone=True))
    """When member's membership on a list ended. This is set when member is removed from a list."""

    # VIESTIM:  This doesn't work in migration for some reason. Maybe figure out if this is needed or fix later.
    # join_method = db.Column(db.Enum(MessageListJoinMethod))
    """How the member came to a list."""

    membership_verified = db.Column(db.DateTime(timezone=True))
    """When the user's joining was verified. If user is added e.g. by a teacher to a course's message list, 
    this date is the date teacher added the member. If the user was invited, then this is the date they verified 
    their join. """

    # VIESTIM: This doesn't strictly speaking exists in the original plan. This acts as an discriminator,
    #  see SQLAlchemy's documentation's term list.
    member_type = db.Column(db.Text)
    """Discriminator for polymorhphic members."""

    message_list = db.relationship("MessageListModel", back_populates="members")
    tim_member = db.relationship("MessageListTimMember", back_populates="member")
    external_member = db.relationship("MessageListExternalMember", back_populates="member")
    distribution = db.relationship("MessageListDistribution", back_populates="member")

    __mapper_args__ = {"polymorphic_identity": "member", "polymorphic_on": member_type}


class MessageListTimMember(MessageListMember):
    """A member of message list who is also a TIM user(group). This can be one person in their own personal user
    group or this can be e.g. a course's group."""

    __tablename__ = "messagelist_tim_member"

    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)

    group_id = db.Column(db.Integer, db.ForeignKey("usergroup.id"))
    """A UserGroup id for a member."""

    member = db.relationship("MessageListMember", back_populates="tim_member")
    user_group = db.relationship("UserGroup", back_populates="messagelist_membership")

    __mapper_args__ = {"polymorphic_identity": "tim_member"}


class MessageListExternalMember(MessageListMember):
    """A member of message list who is *not* a TIM user. Mainly intended for, but not necessary limited to,
    email-list only usage."""

    __tablename__ = "messagelist_external_member"

    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)

    email_address = db.Column(db.Text, unique=True)
    """Email address of message list's external member."""

    member = db.relationship("MessageListMember", back_populates="external_member")

    __mapper_args__ = {"polymorphic_identity": "external_member"}


class MessageListDistribution(db.Model):
    """Message list member's chosen distribution channels."""

    __tablename__ = "messagelist_distribution"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)

    channel = db.Column(db.Enum(Channel))
    """Which message channels are used for a message list."""

    member = db.relationship("MessageListMember", back_populates="distribution")


class UserEmails(db.Model):
    """TIM users' additional emails."""

    __tablename__ = "user_emails"

    id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)

    additional_email = db.Column(db.Text, unique=True)
    """The users another email."""

    is_primary_email = db.Column(db.Boolean)
    """Which one of the additional emails is user's primary email address. If none are, the Sisu given email address 
    is assumed to be primary email address."""

    address_verified = db.Column(db.DateTime(timezone=True))
    """The user has to verify they are in the possession of the email address."""

    # VIESTIM: Do we need a relationship to useraccount table?


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
