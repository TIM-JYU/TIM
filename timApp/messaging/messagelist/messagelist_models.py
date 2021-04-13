from enum import Enum

from timApp.messaging.messagelist.listoptions import ArchiveType
from timApp.timdb.sqa import db


class Channel(Enum):
    """The message channels TIM uses."""
    TIM_MESSAGE = 1
    EMAIL_LIST = 2
    # EMAIL = 3


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
    id = db.Column(db.Integer)
    message_list_id = db.Column(db.Integer, db.ForeignKey("messagelist.id"))
    can_send = db.Column(db.Bool)


class MessageListTimMember(db.Model):
    __tablename__ = "messagelist_tim_member"
    id = db.Column(db.Integer, db.ForeignKey("usergroup.id"))


class MessageListExternalMember(db.Model):
    __tablename__ = "messagelist_external_member"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"))
    email_address = db.Model(db.Text)


class MessageListDistribution(db.Model):
    __tablename__ = "messagelist_distribution"
    id = db.Column(db.Integer, db.ForeignKey("messagelist_member.id"))
    channel_id = db.Column(db.Enum(Channel))
