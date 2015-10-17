from contracts import contract
from timdb.tempdbbase import TempDbBase


class NewAnswers(TempDbBase):
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
        self.cursor.execute("""
            INSERT INTO NewAnswer(lecture_id, asked_id, user_id)
            VALUES (%s,%s,%s)
        """, (lecture_id, asked_id, user_id))

        if commit:
            self.db.commit()

    @contract
    def delete_question_answers(self, asked_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        self.cursor.execute("""
            DELETE FROM NewAnswer
            WHERE asked_id = %s
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
        self.cursor.execute("""
            DELETE FROM NewAnswer
            WHERE lecture_id = %s
        """, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def get_new_answers(self, asked_id: "int", commit: "bool"=True):
        self.cursor.execute("""
            SELECT user_id
            FROM NewAnswer
            WHERE asked_id = %s
        """, [asked_id])

        users = self.cursor.fetchall()
        user_ids = []

        for user in users:
            user_ids.append(user['user_id'])

        if user_ids:
            self.cursor.execute("""
                DELETE
                FROM NewAnswer
                WHERE asked_id = %s AND user_id IN %s
            """, (asked_id, tuple(user_ids)))

            if commit:
                self.db.commit()

        return user_ids
