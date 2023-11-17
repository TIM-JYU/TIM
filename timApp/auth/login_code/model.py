"""
Database models for managing login codes
"""
from datetime import timedelta
from typing import Optional
import enum
import time
from sqlalchemy import ForeignKey, String, Enum
from sqlalchemy.orm import Mapped, mapped_column, registry

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz
from timApp.util.utils import get_current_time

mapper_registry = registry()


class ActivationStatus(enum.Enum):
    Inactive = 0
    Active = 1


class UserLoginCode(db.Model):
    """
    A (temporary) login code for a User.

    Login codes are a simple login method for Users. Login codes are intended for temporary, short-lived use,
    such as users or groups who do not routinely use or log in to TIM, or irregular events like exams.
    Login codes can be generated for users via the :ref:`timApp/static/scripts/tim/ui/group-management.component.ts`
    component when embedded in a suitable document.

    .. note:: Login codes should be purged from the database as soon as they are no longer needed,
              even when they have a set lifetime.
    .. note:: Availability of the login code functionality is controlled
              via the server config setting :attr:`timApp.defaultconfig.LOGINCODES_ENABLE`.
    """

    code: Mapped[str] = mapped_column(String(9), unique=True, primary_key=True)
    """User's temporary login code.
    
    .. note:: Login codes consist of only the numbers 0-9, but we want to store
              them as strings for easier handling elsewhere (eg. formatting for the UI).
    """

    id: Mapped[int] = mapped_column(
        ForeignKey("usergroup.id"), unique=True, primary_key=True
    )
    """User or group that is linked to this login code.
    
       We likely need this to be unique as well, to prevent users having multiple login_codes.
    """

    extra_info: Mapped[str] = mapped_column(String)
    """Additional information on the user, for example, the name of a real-world class or 'homegroup' they belong to."""

    activation_start: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp for the earliest time that the login code can be used for logging into TIM."""

    activation_end: Mapped[Optional[datetime_tz]] = mapped_column()
    """Timestamp for the latest time that the login code can be used for logging into TIM.
    
    .. note:: Activation end time should always be required to be set when new login codes are generated.
              It may be used to determine when users' sessions should be invalidated (forcing users to use
              a different login method), or a point in time when users' login codes are deleted from the
              database.
    """

    activation_status: Mapped[int] = mapped_column(
        default=ActivationStatus.Inactive.value
    )
    """ActivationStatus of the login code, for manually setting the activation status of the login code."""

    def expire(self, time_offset: timedelta | None = None) -> None:
        delta = time_offset if time_offset else timedelta(seconds=0)
        self.activation_end = get_current_time() - delta
        self.activation_status = ActivationStatus.Inactive.value

    @staticmethod
    def create(
        _id: int,
        extra_info: str | None = None,
        activation_start: datetime_tz | None = None,
        activation_end: datetime_tz | None = None,
        activation_status: ActivationStatus | None = None,
    ) -> "UserLoginCode":
        """Creates a new login code for a user.

        :param _id: Usergroup id that this login code is linked to.
        :param extra_info: optional extra information on the linked user.
        :param activation_start: optional time when the code should be activated,
                                 ie. earliest time when the code can be used.
        :param activation_end: optional time when the code should expire.
        :param activation_status: activation status of the code.
        :returns: Created UserLoginCode object.
        """

        ulc = UserLoginCode(
            code=UserLoginCode.generate_code(),
            id=_id,
            extra_info=extra_info,
            activation_start=activation_start,
            activation_end=activation_end,
            activation_status=activation_status,
        )
        db.session.add(ulc)
        return ulc

    @staticmethod
    def generate_code() -> str:
        millis = time.time_ns() / 1_000_000
        code = str(int(millis))
        # last 9 digits, this flips around every ~11.5 days
        # which should be fine for temporary use
        code = code[len(code) - 10 : -1]
        return code
