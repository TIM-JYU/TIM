__author__ = 'hajoviin'
from contracts import contract
from timdb.timdbbase import TimDbBase


class LectureAnswers(TimDbBase):
    @contract
    def add_answer(self, user_id: "int", question_id: "int", lecture_id:"int", answer: "string", answered_on: "string",
                   points: "float",
                   commit: 'bool'=True):
        cursor = self.db.cursor()

        cursor.execute("""
            INSERT INTO LectureAnswer(user_id, question_id,lecture_id, answer, answered_on,points)
            VALUES (?,?,?,?,?,?)
        """, [user_id, question_id, lecture_id, answer, answered_on, points])

        if commit:
            self.db.commit()

    def get_answers_to_question(self, question_id: "int", timestamp: "string") -> 'list(dict)':
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT answer
            FROM LectureAnswer
            WHERE question_id = ? AND answered_on > ?
        """, [question_id, timestamp])

        return self.resultAsDictionary(cursor)


    @contract
    def get_answers_to_questions_from_lecture(self, lecture_id: "int") -> "list(dict)":
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT *
                        FROM LectureAnswer
                        WHERE lecture_id = ?
        """, [lecture_id])

        return self.resultAsDictionary(cursor)

    @contract
    def delete_answers_from_question(self, question_id: "int", commit:"bool"=True):
        cursor = self.db.cursor()

        cursor.execute("""
                    DELETE FROM LectureAnswer
                    WHERE question_id = ?
        """, [question_id])

        if commit:
            self.db.commit()
