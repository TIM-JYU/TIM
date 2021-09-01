from enum import Enum

from timApp.timdb.sqa import db


class VerificationType(Enum):
    """Type of verification, used to direct the proper verification action afterwards."""
    LIST_JOIN = "list"
    """Add user to the message list."""
    CONTACT_OWNERSHIP = "contact"
    """Add a new contact to the user."""


class Verification(db.Model):
    """For various pending verifications, such as message list joining and contact information ownership
    verification."""

    __tablename__ = "verification"

    verification_token = db.Column(db.Text, primary_key=True)
    """Verification token used for action verification"""

    verification_type = db.Column(db.Enum(VerificationType), primary_key=True)
    """The type of verification, see VerificationType class for details."""

    verification_pending = db.Column(db.DateTime(timezone=True))
    """When a verification has been added to db, pending sending to a user."""

    verified_at = db.Column(db.DateTime(timezone=True))
    """When the user used the link to verify."""

    contact = db.relationship("UserContact", lazy="select", uselist=False)
    """Relationship to UserContact, to allow connecting without db flushing first."""

    __mapper_args__ = {
        "polymorphic_on": verification_type
    }


class ContactAddVerification(Verification):
    contact_id = db.Column(db.Integer, db.ForeignKey("usercontact.id"))

    __mapper_args__ = {
        "polymorphic_identity": VerificationType.CONTACT_OWNERSHIP
    }
