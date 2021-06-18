from timApp.messaging.messagelist.listoptions import Channel
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

    # TODO: Figure if this relationship is needed.
    # verification = db.relationship("Verification", back_populates="contact", lazy="select", uselist=False)
