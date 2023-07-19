"""
Database models for session management.
"""
from datetime import datetime

from sqlalchemy.ext.hybrid import hybrid_property  # type: ignore

from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


class UserSession(db.Model):
    """
    User session. A session is given to the user when they log in.

    The session is used to track the user's login status and login history:

    .. note:: At the moment, the model is only used for logging when
              :attr:`timApp.defaultconfig.SESSIONS_ENABLE` is set.
    """

    __tablename__ = "usersession"
    __allow_unmapped__ = True

    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
    """
    User ID of the user who owns the session.
    """

    session_id = db.Column(db.Text, primary_key=True)
    """
    Unique session ID.
    """

    logged_in_at = db.Column(db.DateTime, nullable=False, default=get_current_time)
    """
    The time when the user logged in and the session was created.
    """

    expired_at: datetime | None = db.Column(db.DateTime, nullable=True)
    """
    The time when the session was expired.
    """

    origin = db.Column(db.Text, nullable=False)
    """
    Information about the origin of the session.
    May include user agent and any other information about login state.
    """

    user = db.relationship("User", back_populates="sessions")
    """
    User that owns the session. Relationship to :attr:`user_id`.
    """

    def _get_expired(self) -> bool:
        """
        :return: Whether the user session is expired.
        """
        # == is needed because this is a hybrid property
        # noinspection PyComparisonWithNone
        return self.expired_at != None  # noqa: E712

    expired = hybrid_property(_get_expired)
    """Whether the user session is expired."""

    def expire(self) -> None:
        """
        Expires the current user session.
        """
        self.expired_at = get_current_time()
