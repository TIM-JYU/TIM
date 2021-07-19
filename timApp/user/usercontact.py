from typing import List

from timApp.messaging.messagelist.listoptions import Channel
from timApp.timdb.sqa import db
from timApp.user.user import User

from tim_common.marshmallow_dataclass import dataclass


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


@dataclass
class ContactInfo:
    """User contact information. Simplified version of a UserContact row."""
    channel: Channel
    primary: bool
    contact: str


def user_contact_infos(user: User) -> List[ContactInfo]:
    """Get all additional verified user contact informations for a user."""
    user_verified_contacts = UserContact.query.filter_by(user_id=user.id, verified=True).all()
    contacts = [ContactInfo(contact.channel, contact.primary, contact.contact) for contact
                in user_verified_contacts]
    return contacts
