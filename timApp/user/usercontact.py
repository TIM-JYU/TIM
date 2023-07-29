from enum import Enum
from typing import Optional, TYPE_CHECKING, List

from sqlalchemy import UniqueConstraint, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.messaging.messagelist.listinfo import Channel
from timApp.timdb.sqa import db
from timApp.timdb.types import DbModel

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.user.verification.verification import ContactAddVerification


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


class UserContact(DbModel):
    """TIM users' additional contact information."""

    __table_args__ = (
        # A user should not have the same contact for the channel
        # Different users are fine though
        UniqueConstraint("user_id", "contact", "channel", name="user_contact_uc"),
        # The same user cannot have multiple primary contacts for the same channel
        UniqueConstraint(
            "user_id",
            "channel",
            "primary",
            name="user_primary_contact_uc",
            initially="DEFERRED",  # Allow for easy swapping of primary email within the same transaction
        ),
        # Multiple users cannot have the same contact as primary
        UniqueConstraint(
            "channel",
            "contact",
            "primary",
            name="all_users_primary_contact_uc",
        ),
    )

    id: Mapped[int] = mapped_column(primary_key=True)

    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    """Which user owns this contact information."""

    contact: Mapped[str]
    """Contact identifier for a channel."""

    channel: Mapped[Channel]
    """Channel the contact information points to."""

    verified: Mapped[bool] = mapped_column(default=False)
    """Whether this contact info is verified by the user.
    
    If False, the user has made a claim for a contact info, but has not yet verified it's ownership."""

    primary: Mapped[Optional[PrimaryContact]]
    """Whether the contact is primary for the user"""

    contact_origin: Mapped[ContactOrigin]
    """How the contact was added."""

    user: Mapped["User"] = relationship(back_populates="contacts")
    """User that the contact is associated with."""

    _verifications: Mapped[List["ContactAddVerification"]] = relationship(
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
