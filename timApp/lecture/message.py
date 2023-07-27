from datetime import datetime
from typing import TYPE_CHECKING

from sqlalchemy.orm import mapped_column, Mapped

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.lecture.lecture import Lecture
    from timApp.user.user import User

class Message(db.Model):
    __tablename__ = "message"

    msg_id: Mapped[int] = mapped_column(primary_key=True)
    lecture_id: Mapped[int] = mapped_column(db.ForeignKey("lecture.lecture_id"))
    user_id: Mapped[int] = mapped_column(db.ForeignKey("useraccount.id"))
    message: Mapped[str]
    timestamp: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)

    lecture: Mapped["Lecture"] = db.relationship(back_populates="messages", lazy="select")
    user: Mapped["User"] = db.relationship(back_populates="messages", lazy="select")

    def to_json(self):
        return {
            "msg_id": self.msg_id,
            "message": self.message,
            "timestamp": self.timestamp,
            "user": self.user,
        }
