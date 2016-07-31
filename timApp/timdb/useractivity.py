from timdb.tempdbbase import TempDbBase
from sqlalchemy.exc import IntegrityError


class UserActivity(TempDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    def update_or_add_activity(self, lecture_id: int, user_id: int, active: str):
        activity = self.table(lecture_id, user_id, active)
        try:
            self.session.merge(activity)
            self.session.commit()
        except IntegrityError:
            print("Activity already exists.")
            self.session.rollback()


    def delete_lecture_activity(self, lecture_id: int):
        self.table.query.filter_by(lecture_id=lecture_id).delete()
        self.session.commit()

    def get_user_activity(self, lecture_id: int, user_id: int):
        activities = self.table.query.filter_by(user_id=user_id, lecture_id=lecture_id)
        activity = activities.first()
        return activity

    def get_all_user_activity(self, lecture_id: int):
        activities = self.table.query.filter_by(lecture_id=lecture_id)
        return activities
