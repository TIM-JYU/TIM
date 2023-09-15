"""
Database models for session management.
"""
from datetime import datetime
from typing import Optional, TYPE_CHECKING

from sqlalchemy import ForeignKey
from sqlalchemy.ext.hybrid import hybrid_property
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time

if TYPE_CHECKING:
    from timApp.user.user import User


class UserSession(db.Model):
    """
    User session. A session is given to the user when they log in.

    The session is used to track the user's login status and login history:

    .. note:: At the moment, the model is only used for logging when
              :attr:`timApp.defaultconfig.SESSIONS_ENABLE` is set.
    """

    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"), primary_key=True)
    """
    User ID of the user who owns the session.
    """

    session_id: Mapped[str] = mapped_column(primary_key=True)
    """
    Unique session ID.
    """

    logged_in_at: Mapped[datetime] = mapped_column(default=get_current_time)
    """
    The time when the user logged in and the session was created.
    """

    expired_at: Mapped[Optional[datetime]]
    """
    The time when the session was expired.
    """

    origin: Mapped[str]
    """
    Information about the origin of the session.
    May include user agent and any other information about login state.
    """

    user: Mapped["User"] = relationship("User", back_populates="sessions")
    """
    User that owns the session. Relationship to :attr:`user_id`.
    """

    @hybrid_property
    def expired(self) -> bool:
        """
        Whether the user session is expired.

        :return: Whether the user session is expired.
        """
        # == is needed because this is a hybrid property
        # noinspection PyComparisonWithNone
        return self.expired_at != None  # noqa: E712

    def expire(self) -> None:
        """
        Expires the current user session.
        """
        self.expired_at = get_current_time()
