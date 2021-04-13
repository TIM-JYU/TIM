from enum import Enum

from timApp.messaging.messagelist.listoptions import ArchiveType
from timApp.timdb.sqa import db


class Channel(Enum):
    """The message channels TIM uses."""
    TIM_MESSAGE = 1
    EMAIL_LIST = 2
    # EMAIL = 3


# VIESTIM: The dabase models for message lists. Primarily follow the database plan, if you should deviate from the plan
#  document it so after the project it's easier to update the plan for maintainers.

class MessageListModel(db.Model):
    """Database model for message lists"""
    __tablename__ = "messagelist"
    id = db.Column(db.Integer, primary_key=True)
    manage_doc_id = db.Column(db.Integer, db.ForeignKey("block.id"))
    name = db.Column(db.Text)
    can_unsubscribe = db.Column(db.Bool)
    archive = db.Column(db.Enum(ArchiveType))


class MessageListMember(db.Model):
    """Database model for members of a message list."""
    __tablename__ = "messagelist_member"
    id = db.Column(db.Integer, primary_key=True)
    message_list_id = db.Column(db.Integer, db.ForeignKey("messagelist.id"))
    # VIESTIM: This is can_send in the original database plan.
    send_right = db.Column(db.Bool)
    # VIESTIM: delivery_right doesn't exist in the original plan.
    delivery_right = db.Column(db.Bool)


class MessageListTimMember(db.Model):
    """A member of message list who is also a TIM user."""
    __tablename__ = "messagelist_tim_member"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)
    group_id = db.Column(db.Integer, db.ForeignKey("usergroup.id"))


class MessageListExternalMember(db.Model):
    """A member of message list who is *not* a TIM user. Mainly intended for, but not necessary limited to,
    email-list only usage."""
    __tablename__ = "messagelist_external_member"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)
    email_address = db.Model(db.Text)


class MessageListDistribution(db.Model):
    """Message list member's chosen distribution channels."""
    __tablename__ = "messagelist_distribution"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"), primary_key=True)
    channel_id = db.Column(db.Enum(Channel))
