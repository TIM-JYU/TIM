from contracts import contract
from timdb.timdbbase import TimDbBase


class UsersAnswered(TimDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def add_answered_info(self, lecture_id: "int", asked_id: "int", user_id: "int", commit: 'bool'=True):
        """
        Adds info that question has been extended for user
        :param asked_id: asked question id
        :param user_id: user id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM UserAnswered
            WHERE asked_id = ? AND user_id = ?
        """, [asked_id, user_id])
        cursor.execute("""
            INSERT INTO UserAnswered(lecture_id, asked_id, user_id)
            VALUES (?,?,?)
        """, [lecture_id, asked_id, user_id])

        if commit:
            self.db.commit()

    @contract
    def delete_all_from_question(self, asked_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM UserAnswered
            WHERE asked_id = ?
        """, [asked_id])

        if commit:
            self.db.commit()

    @contract
    def delete_all_from_lecture(self, lecture_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM UserAnswered
            WHERE lecture_id = ?
        """, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def user_has_answered(self, asked_id: "int", user_id: "int"):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT *
            FROM UserAnswered
            WHERE asked_id = ? AND user_id = ?
        """, [asked_id, user_id])

        questions = cursor.fetchall()
        return len(questions) > 0
