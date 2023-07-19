from enum import Enum

from timApp.messaging.messagelist.listinfo import Channel
from timApp.timdb.sqa import db


class ContactOrigin(Enum):
    """Indicates what system added the contact to the user.

    The system is also responsible for managing the contact.
    """

    Custom = 1
    Sisu = 2
    Haka = 3


NO_AUTO_VERIFY_ORIGINS = {ContactOrigin.Custom}
"""Origins that must not be automatically verified when added to user"""


class PrimaryContact(Enum):
    """Whether the contact is primary.

    Enum should have only one value which is used to enforce the unique constraint.
    """

    true = True


class UserContact(db.Model):
    """TIM users' additional contact information."""

    __tablename__ = "usercontact"
    __allow_unmapped__ = True

    __table_args__ = (
        # A user should not have the same contact for the channel
        # Different users are fine though
        db.UniqueConstraint("user_id", "contact", "channel", name="user_contact_uc"),
        # The same user cannot have multiple primary contacts for the same channel
        db.UniqueConstraint(
            "user_id",
            "channel",
            "primary",
            name="user_primary_contact_uc",
            initially="DEFERRED",  # Allow for easy swapping of primary email within the same transaction
        ),
        # Multiple users cannot have the same contact as primary
        db.UniqueConstraint(
            "channel",
            "contact",
            "primary",
            name="all_users_primary_contact_uc",
        ),
    )

    id = db.Column(db.Integer, primary_key=True)

    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    """Which user owns this contact information."""

    contact = db.Column(db.Text, nullable=False)
    """Contact identifier for a channel."""

    channel = db.Column(db.Enum(Channel), nullable=False)
    """Channel the contact information points to."""

    verified = db.Column(db.Boolean, nullable=False, default=False)
    """Whether this contact info is verified by the user.
    
    If False, the user has made a claim for a contact info, but has not yet verified it's ownership."""

    primary = db.Column(db.Enum(PrimaryContact))
    """Whether the contact is primary for the user"""

    contact_origin: ContactOrigin = db.Column(db.Enum(ContactOrigin), nullable=False)
    """How the contact was added."""

    user = db.relationship("User", back_populates="contacts", lazy="select")
    """User that the contact is associated with."""

    _verifications = db.relationship(
        "ContactAddVerification",
        back_populates="contact",
        cascade="all, delete-orphan",
    )

    def to_json(self) -> dict:
        return {
            "contact": self.contact,
            "channel": self.channel,
            "verified": self.verified,
            "origin": self.contact_origin,
            "primary": self.primary == PrimaryContact.true,
        }

    def __repr__(self) -> str:
        values = ", ".join([f"{k}={v}" for k, v in self.to_json().items()])
        return f"<UserContact(id={self.id}, {values}>"
