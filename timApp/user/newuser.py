from sqlalchemy import func

from timApp.timdb.sqa import db
from timApp.user.userutils import check_password_hash


class NewUser(db.Model):
    """A user that is going to register to TIM via email and has not yet completed the registration process."""

    __tablename__ = "newuser"
    __allow_unmapped__ = True
    
    email = db.Column(db.Text, primary_key=True)
    """Email address."""

    pass_ = db.Column("pass", db.Text, nullable=False, primary_key=True)
    """Password hash for the temporary password."""

    created = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())
    """The time when user clicked "Sign up"."""

    def check_password(self, password: str) -> bool:
        return check_password_hash(password, self.pass_)
