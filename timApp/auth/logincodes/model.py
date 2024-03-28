"""
Models for user login codes.
"""
import random
import secrets
import string
from dataclasses import dataclass
from typing import Optional, TYPE_CHECKING

from sqlalchemy import ForeignKey, insert, select, func, String
from sqlalchemy.orm import Mapped, mapped_column, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

from timApp.user.user import User

# TODO: This should be probably a config option
CODE_LENGTH = 12


class UserLoginCode(db.Model):
    """
    A temporary login code for a user.
    A user can be authenticated with an active login code.

    A user can have multiple codes assigned to them, but only the
    active codes are valid for authentication.

    """

    code: Mapped[str] = mapped_column(primary_key=True)
    """
    User login code.
    The login code is unique for each user.
    """

    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    """
    User ID of the user to which the code is assigned.
    """

    active_from: Mapped[Optional[datetime_tz]]
    """
    The time when the code becomes active.
    If not set, the code is not active.
    """

    active_to: Mapped[datetime_tz]
    """
    The time when the code becomes inactive.
    This must be always set to prevent code misuse.
    """

    valid: Mapped[bool] = mapped_column(default=True)
    """
    Whether the code is valid for authentication.
    This is used for soft-deleting the codes and quickly invalidating them.
    """

    name: Mapped[Optional[str]]
    """
    Optional name/description for the code.
    Can be used for various filtering and logging purposes.
    """

    session_code: Mapped[str] = mapped_column(unique=True, index=True)
    """
    Session code for the user login code.
    The session code is used for attaching sessions to user codes without exposing the codes to the session directly.
    Each session code is unique.
    """

    user: Mapped["User"] = relationship("User")
    """
    The user to which the code is assigned.
    """

    @staticmethod
    def generate_new(
        user: User,
        active_to: datetime_tz,
        active_from: Optional[datetime_tz] = None,
        name: Optional[str] = None,
        valid: bool = True,
    ) -> "UserLoginCode":
        """
        Generate a new user login code for the user.

        :param user: User for which the code is created.
        :param active_to: How long the code is active.
        :param active_from: When the code becomes active. If None, the code is not active at all.
        :param name: Optional name/description for the code.
        :param valid: Whether the code is valid for authentication.
        :return: New user login code that is guaranteed to be unique.
        """
        attempts = 0
        while True:
            try:
                with db.session.begin_nested() as sess:
                    digits = list(string.digits)
                    random.shuffle(digits)
                    new_code = "".join(
                        secrets.choice(digits) for _ in range(CODE_LENGTH)
                    )
                    lc = UserLoginCode(
                        code=new_code,
                        user=user,
                        active_to=active_to,
                        active_from=active_from,
                        name=name,
                        session_code=secrets.token_urlsafe(32),
                        valid=valid,
                    )
                    db.session.add(lc)
                    sess.commit()
                    return lc
            except Exception as e:
                attempts += 1
                if attempts > 100:
                    raise IOError("Failed to generate a unique login code") from e
                continue
