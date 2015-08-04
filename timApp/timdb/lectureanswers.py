__author__ = 'hajoviin'
from contracts import contract
from timdb.timdbbase import TimDbBase


class LectureAnswers(TimDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def add_answer(self, user_id: "int", question_id: "int", lecture_id: "int", answer: "str", answered_on: "str",
                   points: "float", commit: 'bool'=True):
        """
        Adds answer to lecture question
        :param user_id: user id
        :param question_id: question id
        :param lecture_id: lecture id
        :param answer: answer
        :param answered_on: time of the answer
        :param points: points from the anwer
        :param commit: commit the database
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            INSERT INTO LectureAnswer(user_id, question_id,lecture_id, answer, answered_on,points)
            VALUES (?,?,?,?,?,?)
        """, [user_id, question_id, lecture_id, answer, answered_on, points])

        if commit:
            self.db.commit()

    @contract
    def update_answer(self, answer_id: 'int', user_id: "int", question_id: "int", lecture_id: "int", answer: "str",
                      answered_on: "str", points: "float", commit: 'bool'=True):
        """
        Update users answer to question
        :param answer_id: answer id
        :param user_id: user id
        :param question_id: question id
        :param lecture_id: lecture id
        :param answer: answer
        :param answered_on: time of the answer
        :param points: points from the anwer
        :param commit: commit the database
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            UPDATE LectureAnswer
            SET user_id = ?, question_id = ?, lecture_id = ?, answer = ?, answered_on = ?, points = ?
            WHERE answer_id = ?
        """, [user_id, question_id, lecture_id, answer, answered_on, points, answer_id])

        if commit:
            self.db.commit()

    def get_answers_to_question(self, question_id: "int", timestamp: "string") -> 'list(dict)':
        """
        Gets answers from specific question
        :param question_id: question id
        :param timestamp: gets answer that came after this time.
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT answer
            FROM LectureAnswer
            WHERE question_id = ? AND answered_on > ?
        """, [question_id, timestamp])

        return self.resultAsDictionary(cursor)

    def get_user_answer_to_question(self, asked_id: "int", user_id: "int") -> 'list(dict)':
        """
        Gets users answer to specific question
        :param asked_id: asked question id
        :param user_id: user id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT answer_id, answer
            FROM LectureAnswer
            WHERE question_id = ? AND user_id = ?
        """, [asked_id, user_id])

        return self.resultAsDictionary(cursor)


    @contract
    def get_answers_to_questions_from_lecture(self, lecture_id: "int") -> "list(dict)":
        """
        Gets all the naswers to questions from specific lecture
        :param lecture_id: lecture ID
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT *
                        FROM LectureAnswer
                        WHERE lecture_id = ?
        """, [lecture_id])

        return self.resultAsDictionary(cursor)

    @contract
    def get_user_answers_to_questions_from_lecture(self, lecture_id: "int", user_id: "int") -> "list(dict)":
        """
        Gets all the naswers to questions from specific lecture
        :param lecture_id: lecture ID
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT *
                        FROM LectureAnswer
                        WHERE lecture_id = ? AND user_id = ?
        """, [lecture_id, user_id])

        return self.resultAsDictionary(cursor)

    @contract
    def delete_answers_from_question(self, question_id: "int", commit:"bool"=True):
        """
        Deletes answers from question
        :param question_id: question
        :param commit: commit to database
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
                    DELETE FROM LectureAnswer
                    WHERE question_id = ?
        """, [question_id])

        if commit:
            self.db.commit()
