from contracts import contract
from timdb.timdbbase import TimDbBase


class NewAnswers(TimDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def user_answered(self, lecture_id: "int", asked_id: "int", user_id: "int", commit: 'bool'=True):
        """
        Adds info that question has been extended for user
        :param asked_id: asked question id
        :param user_id: user id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            INSERT INTO NewAnswer(lecture_id, asked_id, user_id)
            VALUES (?,?,?)
        """, [lecture_id, asked_id, user_id])

        if commit:
            self.db.commit()

    @contract
    def delete_question_answers(self, asked_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM NewAnswer
            WHERE asked_id = ?
        """, [asked_id])

        if commit:
            self.db.commit()

    @contract
    def delete_lecture_answers(self, lecture_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM NewAnswer
            WHERE lecture_id = ?
        """, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def get_new_answers(self, asked_id: "int", commit: "bool"=True):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT user_id
            FROM NewAnswer
            WHERE asked_id = ?
        """, [asked_id])

        users = cursor.fetchall()
        user_ids = []

        users_str = []
        for user in users:
            user_id = user['user_id']
            user_ids.append(user_id)
            users_str.append(str(user_id))

        users_str = ', '.join(users_str)
        cursor.execute("""
            DELETE
            FROM NewAnswer
            WHERE asked_id = ? AND user_id IN (?)
        """, [asked_id, users_str])

        if commit:
            self.db.commit()

        return user_ids
