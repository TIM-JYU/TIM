from enum import Enum

from timApp.timdb.sqa import db


class VerificationType(Enum):
    """Type of verification, used to direct the proper verification action afterwards."""
    LIST_JOIN = 1
    """A user has been invited to a message list."""
    CONTACT_OWNERSHIP = 2
    """A user has added a new contact information for themselves, and it's verified they are in possession of said 
    contact information. """


class Verification(db.Model):
    """For various pending verifications, such as message list joining and contact information ownership
    verification."""

    __tablename__ = "verifications"

    id = db.Column(db.Integer, primary_key=True)

    contact_id = db.Column(db.Integer, db.ForeignKey("user_contact.id"))

    verification_type = db.Column(db.Enum(VerificationType), nullable=False)
    """The type of verification, see VerificationType class for details."""

    verification_pending = db.Column(db.DateTime(timezone=True))
    """When a verification has been added to db, pending sending to a user."""

    verification_token = db.Column(db.Text, nullable=False)
    """Generated verification link. This is given to the user and once they click on it, they are verified (in 
    whatever it was that needed verification)."""

    verified_at = db.Column(db.DateTime(timezone=True))
    """When the user used the link to verify."""

    contact = db.relationship("UserContact", lazy="select", uselist=False)
    """Relationship to UserContact, to allow connecting without db flushing first."""
