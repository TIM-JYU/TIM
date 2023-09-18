"""
Database models for managing login codes
"""
from datetime import datetime

from sqlalchemy.ext.hybrid import hybrid_property  # type: ignore

from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


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
