from typing import Dict

from timApp.messaging.messagelist.listinfo import Channel
from timApp.timdb.sqa import db


class UserContact(db.Model):
    """TIM users' additional contact information."""

    __tablename__ = "user_contact"

    id = db.Column(db.Integer, primary_key=True)

    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    """Which user owns this contact information."""

    contact = db.Column(db.Text, nullable=False)
    """Contact identifier for a channel."""

    channel = db.Column(db.Enum(Channel), nullable=False)
    """Channel the contact information points to."""

    primary = db.Column(db.Boolean, nullable=False)
    """If a contact information is a fallback contact to be used is cases where a user has not configured other 
    contact information for a specific channel or a message list. """

    verified = db.Column(db.Boolean, nullable=False)
    """The user has to verify they are in the possession of the contact information.
    
    For a value of False, this means that a user has made a claim for a contact info, but has not yet verified it's 
    ownership. """

    user = db.relationship('User', back_populates='contacts', lazy='select')
    """User that the contact is associated with.
    """

    def to_json(self) -> Dict:
        return {
            "contact": self.contact,
            "channel": self.channel,
            "primary": self.primary,
            "verified": self.verified
        }
