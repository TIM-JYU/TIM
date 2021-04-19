from enum import Enum

from timApp.timdb.sqa import db


class DisplayType(Enum):
    TOP_OF_PAGE = 0
    STICKY = 1


class InternalMessageModel(db.Model):
    """A TIM message."""

    __tablename__ = 'internalmessage'

    id = db.Column(db.Integer, primary_key=True)
    """Message identifier."""

    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    """Block identifier."""

    par_id = db.Column(db.Text, nullable=False)
    """Paragraph identifier."""

    confirm = db.Column(db.Boolean, nullable=False)
    """Whether the message can be confirmed as read."""
    # VIESTIM: confirm, receipt or acknowledge?

    reply = db.Column(db.Boolean, nullable=False)
    """Whether the message can be replied to."""

    display_type = db.Column(db.Enum(DisplayType), nullable=False)
    """How the message is displayed."""

    # TODO: Expiration date and sender if necessary
    #  Expiration date: use Block's BlockAccess: accessible_from and accessible_to?
    #  Sender: use Block's BlockAccess: usergroup_id (owner?)


class InternalMessageDisplayModel(db.Model):
    """Where and for whom a TIM message is displayed."""

    __tablename__ = 'internalmessagedisplay'

    message_id = db.Column(db.Integer, db.ForeignKey('internalmessage.id'), primary_key=True)
    """Message identifier."""

    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'))
    """Who sees the message; if null, displayed for everyone."""

    display_doc_id = db.Columm(db.Integer, db.ForeignKey('block.id'), nullable=False)
    """Identifier for the document or the folder where the message is displayed."""
    # VIESTIM: If null, displayed globally on TIM? We're not doing message approval through admin though.
    #  Currently not nullable.


class InternalMessageConfirmModel(db.Model):
    """Metadata about message confirmations."""

    __tablename__ = 'internalmessageconfirm'

    message_id = db.Column(db.Integer, db.ForeignKey('internalmessage.id'), primary_key=True)
    """Message identifier."""

    user_id = db.Column(db.Integer, db.ForeignKey('user.id'), nullable=False)
    """Identifier for the user who confirmed the message as read."""

    confirmed_on = db.Column(db.DateTime)
    """Timestamp for when the message was confirmed as read."""
