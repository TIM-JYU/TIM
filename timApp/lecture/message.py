from datetime import datetime

from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class Message(db.Model):
    __tablename__ = "message"
    

    msg_id = mapped_column(db.Integer, primary_key=True)
    lecture_id = mapped_column(
        db.Integer, db.ForeignKey("lecture.lecture_id"), nullable=False
    )
    user_id = mapped_column(db.Integer, db.ForeignKey("useraccount.id"), nullable=False)
    message = mapped_column(db.Text, nullable=False)
    timestamp = mapped_column(
        db.DateTime(timezone=True), nullable=False, default=datetime.utcnow
    )

    lecture = db.relationship("Lecture", back_populates="messages", lazy="select")
    user = db.relationship("User", back_populates="messages", lazy="select")

    def to_json(self):
        return {
            "msg_id": self.msg_id,
            "message": self.message,
            "timestamp": self.timestamp,
            "user": self.user,
        }
