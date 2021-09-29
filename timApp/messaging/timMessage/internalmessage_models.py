from enum import Enum
from typing import Any

from timApp.timdb.sqa import db


class DisplayType(Enum):
    TOP_OF_PAGE = 1
    STICKY = 2


class InternalMessage(db.Model):
    """A TIM message."""

    __tablename__ = 'internalmessage'

    id = db.Column(db.Integer, primary_key=True)
    """Message identifier."""

    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    """Block identifier."""

    par_id = db.Column(db.Text, nullable=False)
    """Paragraph identifier."""

    can_mark_as_read = db.Column(db.Boolean, nullable=False)
    """Whether the recipient can mark the message as read."""

    reply = db.Column(db.Boolean, nullable=False)
    """Whether the message can be replied to."""

    display_type = db.Column(db.Enum(DisplayType), nullable=False)
    """How the message is displayed."""

    expires = db.Column(db.DateTime)
    """"When the message display will disappear."""

    replies_to = db.Column(db.Integer)
    """Id of the message which this messages is a reply to"""

    displays = db.relationship('InternalMessageDisplay', back_populates='message')
    readreceipts = db.relationship('InternalMessageReadReceipt', back_populates='message')
    block = db.relationship('Block', back_populates='internalmessage')

    def to_json(self) -> dict[str, Any]:
        return {'id': self.id,
                'doc_id': self.doc_id,
                'par_id': self.par_id,
                'can_mark_as_read': self.can_mark_as_read,
                'reply': self.reply,
                'display_type': self.display_type,
                'displays': self.displays,
                'readreceipts': self.readreceipts,
                }


class InternalMessageDisplay(db.Model):
    """Where and for whom a TIM message is displayed."""

    __tablename__ = 'internalmessage_display'

    id = db.Column(db.Integer, primary_key=True)
    """Message display identifier."""

    message_id = db.Column(db.Integer, db.ForeignKey('internalmessage.id'), nullable=False)
    """Message identifier."""

    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'))
    """Who sees the message; if null, displayed for everyone."""

    display_doc_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    """
    Identifier for the document or the folder where the message is displayed. If null, 
    the message is displayed globally.
    """

    message = db.relationship('InternalMessage', back_populates='displays')
    usergroup = db.relationship('UserGroup', back_populates='internalmessage_display')
    display_block = db.relationship('Block', back_populates='internalmessage_display')

    def to_json(self) -> dict[str, Any]:
        return {'id': self.id,
                'message_id': self.message_id,
                'usergroup_id': self.usergroup_id,
                'display_doc_id': self.display_doc_id,
                }


class InternalMessageReadReceipt(db.Model):
    """Metadata about read receipts."""

    __tablename__ = 'internalmessage_readreceipt'

    rcpt_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), primary_key=True)
    """Message recipient identifier."""

    message_id = db.Column(db.Integer, db.ForeignKey('internalmessage.id'), primary_key=True)
    """Message identifier."""

    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'))
    """Identifier for the user who marked the message as read."""

    marked_as_read_on = db.Column(db.DateTime)
    """Timestamp for when the message was marked as read."""

    recipient = db.relationship('UserGroup', back_populates='internalmessage_readreceipt')
    message = db.relationship('InternalMessage', back_populates='readreceipts')
    user = db.relationship('User', back_populates='internalmessage_readreceipt')

    def to_json(self) -> dict[str, Any]:
        return {'rcpt_id': self.rcpt_id,
                'message_id': self.message_id,
                'user_id': self.user_id,
                'marked_as_read_on': self.marked_as_read_on,
                }
