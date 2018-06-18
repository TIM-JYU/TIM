from timApp.timdb.sqa import db


class LectureUsers(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'lectureusers'
    lecture_id = db.Column(db.Integer, db.ForeignKey('lecture.lecture_id'), primary_key=True)
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'),
                        primary_key=True)