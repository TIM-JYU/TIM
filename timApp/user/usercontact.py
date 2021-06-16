from timApp.timdb.sqa import db


class UserContact(db.Model):
    """TIM users' additional emails."""

    __tablename__ = "user_contacts"

    id = db.Column(db.Integer, primary_key=True)

    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"))
    # db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
    """Which user owns this contact information."""

    contact = db.Column(db.Text)
    """Contact identifier for a channel. Connects the user to their information on a channel."""

    primary = db.Column(db.Boolean)
    """If a contact information is a fallback contact to be used is cases where a user has not configured other 
    contact information for a specific channel or a message list. """

    verified = db.Column(db.Boolean)
    # verified = db.relationship("Verification", back_populates=)
    # address_verified = db.Column(db.DateTime(timezone=True))
    """The user has to verify they are in the possession of the contact information."""
