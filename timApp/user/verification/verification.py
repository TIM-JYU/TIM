import secrets
from enum import Enum
from functools import cache
from typing import Optional, Dict

from flask import render_template_string, url_for

from timApp.document.docentry import DocEntry
from timApp.timdb.sqa import db
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

    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    """User that can react to verification request."""

    requested_at = db.Column(db.DateTime(timezone=True))
    """When a verification has been added to db, pending sending to a user."""

    reacted_at = db.Column(db.DateTime(timezone=True))
    """When the user reacted to verification request."""

    user = db.relationship("User", lazy="select")
    """User that can react to verification request."""

    def approve(self) -> None:
        raise NotImplemented()

    def deny(self) -> None:
        raise NotImplemented()

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

    def deny(self) -> None:
        self.contact.verified = False

    def approve(self) -> None:
        self.contact.verified = True

    def to_json(self) -> Dict:
        return {
            **super().to_json(),
            'channel': self.contact.channel,
            'contact': self.contact.contact,
        }

    __mapper_args__ = {
        "polymorphic_identity": VerificationType.CONTACT_OWNERSHIP
    }


def request_verification_raw(verification: Verification, message_title: str, message_body: str) -> None:
    from timApp.notification.send_email import send_email
    from timApp.tim_app import app

    verification.token = secrets.token_urlsafe(VERIFICATION_TOKEN_LEN)
    verification.requested_at = get_current_time()
    user = verification.user

    url = app.config["TIM_HOST"] + url_for("verify.show_verify_page",
                                           verify_type=verification.type.value,
                                           verify_token=verification.token)
    title = render_template_string(message_title, user=user, verification=verification)
    body = render_template_string(message_body, user=user, verification=verification, verify_url=url)

    db.session.add(verification)
    # TODO: Send to primary email
    send_email(user.email, title, body)


def request_verification(verification: Verification, message_template_doc: str) -> None:
    subject = f"Verify {verification.type.value}"
    body = "{{verify_url}}"

    # noinspection PyBroadException
    try:
        doc = DocEntry.find_by_path(message_template_doc)
        subject = doc.document.get_settings().get("subject", f"Verify {verification.type.value}")
        body = doc.document.export_markdown(export_ids=False, export_settings=False)
    except:
        pass

    return request_verification_raw(verification, subject, body)
