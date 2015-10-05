from contracts import contract
from timdb.timdbbase import TimDbBase


class UsersShown(TimDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def add_shown_info(self, lecture_id: "int", asked_id: "int", user_id: "int", commit: 'bool'=True):
        """
        Adds a running question that is related to lecture
        :param lecture_id: lecture id
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            INSERT INTO UsersShown(lecture_id, asked_id, user_id)
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
            DELETE FROM UsersShown
            WHERE asked_id = ?
        """, [asked_id])

        if commit:
            self.db.commit()

    def delete_all_from_lecture(self, lecture_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM UsersShown
            WHERE lecture_id = ?
        """, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def shown_to_user(self, asked_id: "int", user_id: "int", commit: "bool"=True):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT *
            FROM UsersShown
            WHERE asked_id = ? AND user_id = ?
        """, [asked_id, user_id])

        questions = cursor.fetchall()
        return len(questions) > 0
