import secrets
from enum import Enum
from functools import cache
from typing import Optional

from flask import render_template_string, url_for
from sqlalchemy import select, ForeignKey
from sqlalchemy.orm import load_only, mapped_column, Mapped, relationship

from timApp.document.docentry import DocEntry
from timApp.timdb.sqa import db, run_sql
from timApp.timdb.types import datetime_tz
from timApp.user.user import User
from timApp.user.usercontact import UserContact, PrimaryContact
from timApp.util.utils import get_current_time

VERIFICATION_TOKEN_LEN = 32


class VerificationType(Enum):
    """Type of verification, used to direct the proper verification action afterwards."""

    LIST_JOIN = "list"
    """Add user to the message list."""
    CONTACT_OWNERSHIP = "contact"
    """Add a new contact to the user."""
    SET_PRIMARY_CONTACT = "set_primary_contact"
    """Set a primary contact."""

    @staticmethod
    @cache
    def get_verification_types() -> set[str]:
        return {v.value for v in VerificationType}

    @staticmethod
    def parse(t: str) -> Optional["VerificationType"]:
        if t not in VerificationType.get_verification_types():
            return None
        return VerificationType(t)


VERIFICATION_TEMPLATES = {
    VerificationType.CONTACT_OWNERSHIP: "settings/verify-templates/contact",
    VerificationType.LIST_JOIN: None,
    VerificationType.SET_PRIMARY_CONTACT: "settings/verify-templates/primary-contact",
}


class Verification(db.Model):
    """For various pending verifications, such as message list joining and contact information ownership
    verification."""

    token: Mapped[str] = mapped_column(primary_key=True)
    """Verification token used for action verification"""

    type: Mapped[VerificationType] = mapped_column(primary_key=True)
    """The type of verification, see VerificationType class for details."""

    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    """User that can react to verification request."""

    requested_at: Mapped[Optional[datetime_tz]]
    """When a verification has been added to db, pending sending to a user."""

    reacted_at: Mapped[Optional[datetime_tz]]
    """When the user reacted to verification request."""

    user: Mapped["User"] = relationship()
    """User that can react to verification request."""

    @property
    def return_url(self) -> str | None:
        return None

    def approve(self) -> None:
        raise NotImplementedError

    def deny(self) -> None:
        raise NotImplementedError

    def to_json(self) -> dict:
        return {"type": self.type}

    __mapper_args__ = {"polymorphic_on": type}


class ContactAddVerification(Verification):
    contact_id: Mapped[Optional[int]] = mapped_column(ForeignKey("usercontact.id"))

    contact: Mapped[Optional["UserContact"]] = relationship()
    """Contact to verify."""

    @property
    def return_url(self) -> str | None:
        return url_for("settings_page.show")

    def deny(self) -> None:
        if not self.contact:
            return
        db.session.delete(self.contact)
        self.contact = None

    def approve(self) -> None:
        if self.contact:
            self.contact.verified = True

    def to_json(self) -> dict:
        if not self.contact:
            return super().to_json()
        return {
            **super().to_json(),
            "channel": self.contact.channel,
            "contact": self.contact.contact,
        }

    __mapper_args__ = {"polymorphic_identity": VerificationType.CONTACT_OWNERSHIP}  # type: ignore


class SetPrimaryContactVerification(ContactAddVerification):
    def deny(self) -> None:
        self.contact = None

    def approve(self) -> None:
        from timApp.messaging.messagelist.emaillist import update_mailing_list_address

        if not self.contact:
            return
        current_primary = (
            run_sql(
                select(UserContact)
                .filter_by(
                    user_id=self.user_id,
                    channel=self.contact.channel,
                    primary=PrimaryContact.true,
                )
                .limit(1)
            )
            .scalars()
            .first()
        )
        with db.session.no_autoflush:
            if current_primary:
                current_primary.primary = None
                # Flush is needed to prevent uniqueness constraint from failing
                db.session.flush()
            self.contact.primary = PrimaryContact.true

            # We update email directly since we already resolved the contact in previous steps
            u = (
                run_sql(
                    select(User)
                    .filter_by(id=self.user_id)
                    .options(load_only(User.email, User.id))
                )
                .scalars()
                .one()
            )
            old_email = u._email
            u._email = self.contact.contact
        update_mailing_list_address(old_email, self.contact.contact)

    __mapper_args__ = {"polymorphic_identity": VerificationType.SET_PRIMARY_CONTACT}  # type: ignore


def send_verification_impl(
    verification: Verification,
    message_title: str,
    message_body: str,
    email: str | None = None,
) -> None:
    from timApp.notification.send_email import send_email
    from timApp.tim_app import app

    user = verification.user
    url = app.config["TIM_HOST"] + url_for(
        "verify.show_verify_page",
        verify_type=verification.type.value,
        verify_token=verification.token,
    )
    title = render_template_string(message_title, user=user, verification=verification)
    body = render_template_string(
        message_body,
        user=user,
        verification=verification,
        verify_url=url,
    )

    send_email(email or user.email, title, body)


def request_verification_impl(
    verification: Verification,
    message_title: str,
    message_body: str,
    email: str | None = None,
) -> None:
    verification.token = secrets.token_urlsafe(VERIFICATION_TOKEN_LEN)
    verification.requested_at = get_current_time()
    db.session.add(verification)

    send_verification_impl(verification, message_title, message_body, email)


def get_verify_message_template(verify_type: VerificationType) -> tuple[str, str]:
    subject = f"Verify {verify_type.value}"
    body = "{{verify_url}}"

    template_path = VERIFICATION_TEMPLATES.get(verify_type, None)

    if template_path:
        # noinspection PyBroadException
        try:
            doc = DocEntry.find_by_path(template_path)
            assert doc is not None
            subject = doc.document.get_settings().get("subject", subject)
            body = doc.document.export_markdown(export_ids=False, export_settings=False)
        except:
            pass

    return subject, body


def request_verification(verification: Verification, email: str | None = None) -> None:
    subject, body = get_verify_message_template(verification.type)
    return request_verification_impl(verification, subject, body, email)


def resend_verification(verification: Verification, email: str | None = None) -> None:
    subject, body = get_verify_message_template(verification.type)
    return send_verification_impl(verification, subject, body, email)
