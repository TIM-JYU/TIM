from contracts import contract
from timdb.timdbbase import TimDbBase


class UserActivity(TimDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def update_or_add_activity(self, lecture_id: "int", user_id: "int", active: "str", commit: 'bool'=True):
        """
        Adds a running question that is related to lecture
        :param lecture_id: lecture id
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            INSERT OR REPLACE INTO UserActivity(lecture_id, user_id, active)
            VALUES (?,?,?)
        """, [lecture_id, user_id, active])

        if commit:
            self.db.commit()

    @contract
    def delete_lecture_activity(self, lecture_id: "int", commit: 'bool'=True):
        """
        Remove all activity from a lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM UserActivity
            WHERE lecture_id = ?
        """, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def get_user_activity(self, lecture_id: "int", user_id: "int", commit: "bool"=True):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT *
            FROM UserActivity
            WHERE lecture = ? AND user_id = ?
        """, [lecture_id, user_id])

        activity = cursor.fetchone()
        return activity

    @contract
    def get_all_user_activity(self, lecture_id: "int", commit: "bool"=True):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT user_id, active
            FROM UserActivity
            WHERE lecture_id = ?
        """, [lecture_id])

        activity = cursor.fetchall()
        return activity
