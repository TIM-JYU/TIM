from enum import Enum

from timApp.messaging.messagelist.listoptions import ArchiveType
from timApp.timdb.sqa import db


class Channel(Enum):
    """The message channels TIM uses and provides for message lists."""
    TIM_MESSAGE = 1
    EMAIL_LIST = 2
    # EMAIL = 3


# VIESTIM: The dabase models for message lists. Primarily follow the database plan, if you should deviate from the plan
#  document it so after the project it's easier to update the plan for maintainers.

class MessageListModel(db.Model):
    """Database model for message lists"""
    __tablename__ = "messagelist"
    id = db.Column(db.Integer, primary_key=True)
    # The message list management document.
    manage_doc_id = db.Column(db.Integer, db.ForeignKey("block.id"))
    name = db.Column(db.Text)
    can_unsubscribe = db.Column(db.Boolean)
    # VIESTIM: archive is type bool in the original plan.
    archive = db.Column(db.Enum(ArchiveType))

    block = db.relationship("Block")
    members = db.relationship("MessageListMember", back_populates="list")


class MessageListMember(db.Model):
    """Database model for members of a message list."""
    __tablename__ = "messagelist_member"
    id = db.Column(db.Integer, primary_key=True)
    message_list_id = db.Column(db.Integer, db.ForeignKey("messagelist.id"))
    # VIESTIM: This is can_send in the original database plan.
    send_right = db.Column(db.Boolean)
    # VIESTIM: delivery_right doesn't exist in the original plan.
    delivery_right = db.Column(db.Boolean)
    # VIESTIM: This doesn't strictly speaking exists in the original plan. This acts as an discriminator,
    #  see SQLAlchemy's documentation's term list.
    member_type = db.Column(db.Text)

    list = db.relationship("MessageListModel", back_populates="members")
    tim_member = db.relationship("MessageListTimMember", back_populates="member")
    external_member = db.relationship("MessageListExternalMember", back_populates="member")
    distribution = db.relationship("MessageListDistribution", back_populates="member")

    __mapper_args__ = {"polymorphic_identity": "member", "polymorphic_on": member_type}


class MessageListTimMember(MessageListMember):
    """A member of message list who is also a TIM user(group). This can be one person in their own personal user
    group or his can be e.g. a course's group."""
    __tablename__ = "messagelist_tim_member"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)
    group_id = db.Column(db.Integer, db.ForeignKey("usergroup.id"))

    member = db.relationship("MessageListMember", back_populates="tim_member")
    # TODO: Add relationship in the UserGroup table.
    user_group = db.relationship("UserGroup", back_populates="messagelist_membership")

    __mapper_args__ = {"polymorphic_identity": "tim_member"}


class MessageListExternalMember(MessageListMember):
    """A member of message list who is *not* a TIM user. Mainly intended for, but not necessary limited to,
    email-list only usage."""
    __tablename__ = "messagelist_external_member"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)
    email_address = db.Column(db.Text, unique=True)

    member = db.relationship("MessageListMember", back_populates="external_member")

    __mapper_args__ = {"polymorphic_identity": "external_member"}


class MessageListDistribution(db.Model):
    """Message list member's chosen distribution channels."""
    __tablename__ = "messagelist_distribution"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)
    channel_id = db.Column(db.Enum(Channel))

    member = db.relationship("MessageListMember", back_populates="distribution")
