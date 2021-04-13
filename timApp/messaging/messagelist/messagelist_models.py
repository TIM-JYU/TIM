from timApp.messaging.messagelist.listoptions import ArchiveType
from timApp.timdb.sqa import db


class MessageListModel(db.Model):
    """Database model for message lists"""
    __tablename__ = "messagelist"
    id = db.Model(db.Integer, primary_key=True)
    manage_doc_id = db.Model(db.Integer, db.ForeignKey("block.id"))
    name = db.Model(db.Text)
    can_unsubscribe = db.Model(db.Bool)
    archive = db.Model(db.Enum(ArchiveType))


class MessageListMember(db.Model):
    pass


class MessageListTimMember(db.Model):
    pass


class MessageListExternalMember(db.Model):
    pass


class MessageListDistribution(db.Model):
    pass
