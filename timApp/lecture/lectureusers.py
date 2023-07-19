from timApp.timdb.sqa import db


class LectureUsers(db.Model):
    __tablename__ = "lectureusers"
    __allow_unmapped__ = True
    
    lecture_id = db.Column(
        db.Integer, db.ForeignKey("lecture.lecture_id"), primary_key=True
    )
    user_id = db.Column(db.Integer, db.ForeignKey("useraccount.id"), primary_key=True)
