from sqlalchemy import func

from timApp.timdb.sqa import db
from timApp.user.userutils import check_password_hash


class NewUser(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'newuser'
    email = db.Column(db.Text, primary_key=True)
    pass_ = db.Column('pass', db.Text, nullable=False)
    created = db.Column(db.DateTime(timezone=True), nullable=False, default=func.now())

    def check_password(self, password: str) -> bool:
        return check_password_hash(password, self.pass_)
