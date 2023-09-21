"""
Database models for managing login codes
"""
from datetime import datetime
from typing import Optional
import enum

from sqlalchemy import ForeignKey, Enum
from sqlalchemy.orm import Mapped, mapped_column

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz
from timApp.util.utils import get_current_time


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

    .. note:: Login codes should be purged from the database as soon as they are no longer needed, even when they have a set lifetime.
    .. note:: Availability of the login code functionality is controlled via the server config setting :attr:`timApp.defaultconfig.LOGINCODES_ENABLE`.
    """

    code: Mapped[int] = mapped_column(primary_key=True)
    """User's temporary login code"""

    user_id: Mapped[int] = mapped_column(ForeignKey("usergroup.id"), primary_key=True)
    """User that is linked to this login code"""

    activation_start: Mapped[Optional[datetime_tz]] = mapped_column(
        default=get_current_time()
    )
    """Timestamp for the earliest time that the login code can be used for logging into TIM."""

    activation_end: Mapped[Optional[datetime_tz]]
    """Timestamp for the latest time that the login code can be used for logging into TIM.
    
    .. note:: Activation end time should always be required to be set when new login codes are generated.
              It may be used to determine when users' sessions should be invalidated (forcing users to use
              a different login method), or a point in time when users' login codes are deleted from the
              database.
    """

    activation_status: Mapped[Enum] = mapped_column(
        default=Enum(ActivationStatus.Inactive)
    )
    """ActivationStatus of the login code, for manually setting the activation status of the login code."""
