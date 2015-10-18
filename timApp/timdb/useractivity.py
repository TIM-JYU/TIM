from contracts import contract
from timdb.tempdbbase import TempDbBase


class UserActivity(TempDbBase):
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
            SELECT *
            FROM UserActivity
            WHERE lecture_id = %s AND user_id = %s
        """, (lecture_id, user_id))

        exists = cursor.fetchall()

        if not exists:
            cursor.execute("""
                INSERT INTO UserActivity(lecture_id, user_id, active)
                VALUES (%s,%s,%s)
            """, (lecture_id, user_id, active))
        else:
            cursor.execute("""
                UPDATE UserActivity
                SET active = %s
                WHERE lecture_id = %s AND user_id = %s
            """, (active, lecture_id, user_id))

        if commit:
            self.db.commit()

    @contract
    def delete_lecture_activity(self, lecture_id: "int", commit: 'bool'=True):
        """
        Remove all activity from a lecture
        :param asked_id: asked questions id
        :return:
        """
        self.cursor.execute("""
            DELETE FROM UserActivity
            WHERE lecture_id = %s
        """, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def get_user_activity(self, lecture_id: "int", user_id: "int", commit: "bool"=True):
        self.cursor.execute("""
            SELECT *
            FROM UserActivity
            WHERE lecture = %s AND user_id = %s
        """, (lecture_id, user_id))

        activity = self.cursor.fetchone()
        return activity

    @contract
    def get_all_user_activity(self, lecture_id: "int", commit: "bool"=True):
        self.cursor.execute("""
            SELECT user_id, active
            FROM UserActivity
            WHERE lecture_id = %s
        """, [lecture_id])

        activity = self.cursor.fetchall()
        return activity
