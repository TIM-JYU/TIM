from sqlalchemy import func
from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db
from timApp.user.userutils import check_password_hash


class NewUser(db.Model):
    """A user that is going to register to TIM via email and has not yet completed the registration process."""

    __tablename__ = "newuser"
    

    email = mapped_column(db.Text, primary_key=True)
    """Email address."""

    pass_ = mapped_column("pass", db.Text, nullable=False, primary_key=True)
    """Password hash for the temporary password."""

    created = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=func.now()
    )
    """The time when user clicked "Sign up"."""

    def check_password(self, password: str) -> bool:
        return check_password_hash(password, self.pass_)
