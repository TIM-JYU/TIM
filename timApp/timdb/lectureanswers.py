__author__ = 'hajoviin'
from contracts import contract
from timdb.timdbbase import TimDbBase


class LectureAnswers(TimDbBase):
    @contract
    def add_answer(self, user_id: "int", question_id: "int", answer: "string", answered_on: "string", points: "float",
                   commit: 'bool'=True):
        cursor = self.db.cursor()

        cursor.execute("""
            INSERT INTO LectureAnswer(user_id, question_id, answer, answered_on,points)
            VALUES (?,?,?,?,?)
        """, [user_id, question_id, answer, answered_on, points])

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