from contracts import contract
from timdb.tempdbbase import TempDbBase
from sqlalchemy.exc import IntegrityError


class UserActivity(TempDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def update_or_add_activity(self, lecture_id: "int", user_id: "int", active: "str"):
        activity = self.table(lecture_id, user_id, active)
        try:
            self.db.session.merge(activity)
            self.db.session.commit()
        except IntegrityError:
            print("Activity already exists.")
            self.db.session.rollback()


    @contract
    def delete_lecture_activity(self, lecture_id: "int"):
        self.table.query.filter_by(lecture_id=lecture_id).delete()
        self.db.session.commit()

    @contract
    def get_user_activity(self, lecture_id: "int", user_id: "int"):
        activities = self.table.query.filter_by(user_id=user_id, lecture_id=lecture_id)
        activity = activities.first()
        return activity

    @contract
    def get_all_user_activity(self, lecture_id: "int"):
        activities = self.table.query.filter_by(lecture_id=lecture_id)
        return activities
