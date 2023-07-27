from sqlalchemy import func
from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz
from timApp.user.userutils import check_password_hash


class NewUser(db.Model):
    """A user that is going to register to TIM via email and has not yet completed the registration process."""

    __tablename__ = "newuser"

    email: Mapped[str] = mapped_column(primary_key=True)
    """Email address."""

    pass_: Mapped[str] = mapped_column("pass", primary_key=True)
    """Password hash for the temporary password."""

    created: Mapped[datetime_tz] = mapped_column(default=func.now())
    """The time when user clicked "Sign up"."""

    def check_password(self, password: str) -> bool:
        return check_password_hash(password, self.pass_)
