from datetime import datetime
from typing import TYPE_CHECKING

from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.timdb.sqa import db
from timApp.timdb.types import datetime_tz

if TYPE_CHECKING:
    from timApp.lecture.lecture import Lecture
    from timApp.user.user import User


class Message(db.Model):
    msg_id: Mapped[int] = mapped_column(primary_key=True)
    lecture_id: Mapped[int] = mapped_column(ForeignKey("lecture.lecture_id"))
    user_id: Mapped[int] = mapped_column(ForeignKey("useraccount.id"))
    message: Mapped[str]
    timestamp: Mapped[datetime_tz] = mapped_column(default=datetime.utcnow)

    lecture: Mapped["Lecture"] = relationship(back_populates="messages", lazy="select")
    user: Mapped["User"] = relationship(back_populates="messages", lazy="select")

    def to_json(self):
        return {
            "msg_id": self.msg_id,
            "message": self.message,
            "timestamp": self.timestamp,
            "user": self.user,
        }
