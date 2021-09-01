import secrets
from enum import Enum
from functools import cache
from typing import Optional, Dict

from flask import render_template_string, url_for

from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usercontact import UserContact
from timApp.util.utils import get_current_time

VERIFICATION_TOKEN_LEN = 32


class VerificationType(Enum):
    """Type of verification, used to direct the proper verification action afterwards."""
    LIST_JOIN = "list"
    """Add user to the message list."""
    CONTACT_OWNERSHIP = "contact"
    """Add a new contact to the user."""

    @staticmethod
    @cache
    def get_verification_types():
        return set(v.value for v in VerificationType)

    @staticmethod
    def parse(t: str) -> Optional["VerificationType"]:
        if t not in VerificationType.get_verification_types():
            return None
        return VerificationType(t)


class Verification(db.Model):
    """For various pending verifications, such as message list joining and contact information ownership
    verification."""

    __tablename__ = "verification"

    token = db.Column(db.Text, primary_key=True)
    """Verification token used for action verification"""

    type: VerificationType = db.Column(db.Enum(VerificationType), primary_key=True)
    """The type of verification, see VerificationType class for details."""

    requested_at = db.Column(db.DateTime(timezone=True))
    """When a verification has been added to db, pending sending to a user."""

    verified_at = db.Column(db.DateTime(timezone=True))
    """When the user used the link to verify."""

    def to_json(self) -> Dict:
        return {
            'type': self.type
        }

    __mapper_args__ = {
        "polymorphic_on": type
    }


class ContactAddVerification(Verification):
    contact_id = db.Column(db.Integer, db.ForeignKey("usercontact.id"))

    contact: UserContact = db.relationship("UserContact", lazy="select", uselist=False)
    """Relationship to UserContact, to allow connecting without db flushing first."""

    def to_json(self) -> Dict:
        return {
            **super().to_json(),
            'channel': self.contact.channel,
            'contact': self.contact.contact,
        }

    __mapper_args__ = {
        "polymorphic_identity": VerificationType.CONTACT_OWNERSHIP
    }


def request_verification(user: User, verification: Verification, message_title: str, message_body: str) -> None:
    from timApp.notification.send_email import send_email
    from timApp.tim_app import app

    verification.token = secrets.token_urlsafe(VERIFICATION_TOKEN_LEN)
    verification.requested_at = get_current_time()

    url = app.config["TIM_HOST"] + url_for("verification.show_verify_page",
                                           verify_type=verification.type.value,
                                           verify_token=verification.token)
    title = render_template_string(message_title, user=user, verification=verification)
    body = render_template_string(message_body, user=user, verification=verification, verify_url=url)

    db.session.add(verification)
    # TODO: Send to primary email
    send_email(user.email, title, body)
