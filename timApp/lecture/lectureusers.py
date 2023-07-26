from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db


class LectureUsers(db.Model):
    __tablename__ = "lectureusers"
    
    
    lecture_id = mapped_column(
        db.Integer, db.ForeignKey("lecture.lecture_id"), primary_key=True
    )
    user_id = mapped_column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
