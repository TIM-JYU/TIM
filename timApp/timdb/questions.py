from datetime import datetime
from typing import List, Optional

from timApp.timdb.tim_models import AskedQuestion, AskedJson, Question
from timApp.timdb.timdbbase import TimDbBase


class Questions(TimDbBase):
    # TODO: Doesn't work until question table has been altered

    def get_question(self, question_id: int) -> List[dict]:
        """Gets question with specific id.

        :param question_id: question id
        :return: the question

        """
        cursor = self.db.cursor()

        cursor.execute(
            """
            SELECT *
            FROM Question
            WHERE question_id = %s
            """, [question_id])

        return self.resultAsDictionary(cursor)

    def add_asked_questions(self, lecture_id: int, doc_id: int, par_id: Optional[str], asked_time: datetime,
                            points: str, asked_json_id: int, expl: str, commit: bool=True) -> int:
        """Creates a new asked questions.

        :param lecture_id: Lecture where question was asked
        :param doc_id: Document, where the question belongs
        :param par_id: Paragraph, where the question belongs
        :param asked_time: The of question
        :param points: Points for answers to question
        :param expl: Explanations to question
        :param asked_json_id: Json of asked question
        :param commit: Commit or not to commit
        :return: The id of the newly created asked question

        """

        aq = AskedQuestion(lecture_id=lecture_id, doc_id=doc_id, par_id=par_id, asked_time=asked_time, points=points,
                           asked_json_id=asked_json_id, expl=expl)
        self.session.add(aq)
        self.session.flush()
        if commit:
            self.session.commit()
        question_id = aq.asked_id
        return question_id

    def update_asked_question_points(self, asked_id: int, points: str, expl: str, commit: bool=True) -> int:
        """Creates a new asked questions.

        :param commit: Commit or not to commit
        :return: The id of the newly created asked question

        """

        cursor = self.db.cursor()
        cursor.execute("""
                       UPDATE AskedQuestion
                       SET points = %s, expl = %s
                       WHERE asked_id = %s
                       """, [points, expl, asked_id])
        if commit:
            self.db.commit()
        return asked_id

    def get_asked_question(self, asked_id: int) -> List[dict]:
        """Gets the asked question by id.

        :return: Questions as a list

        """
        cursor = self.db.cursor()
        cursor.execute(
            """
            SELECT *
            FROM AskedQuestion a
            JOIN AskedJson j
            ON a.asked_json_id = j.asked_json_id
            WHERE a.asked_id = %s
            """, [asked_id])

        return self.resultAsDictionary(cursor)

    def add_asked_json(self, json: str, hash: str, commit: bool=True) -> int:
        """Gets the asked question by id.

        :return: Questions as a list

        """
        aj = AskedJson(json=json, hash=hash)
        self.session.add(aj)
        self.session.flush()
        if commit:
            self.session.commit()
        return aj.asked_json_id

    def get_asked_json_by_id(self, asked_json_id: int) -> List[dict]:
        """Gets the asked question by id.

        :return: Questions as a list

        """
        cursor = self.db.cursor()
        cursor.execute(
            """
            SELECT *
            FROM AskedJson
            WHERE asked_json_id = %s
            """, [asked_json_id])

        return self.resultAsDictionary(cursor)

    def get_asked_json_by_hash(self, asked_hash: str) -> List[dict]:
        """Gets the asked question by id.

        :return: Questions as a list

        """
        cursor = self.db.cursor()
        cursor.execute(
            """
            SELECT *
            FROM AskedJson
            WHERE hash = %s
            """, [asked_hash])

        return self.resultAsDictionary(cursor)

    def get_multiple_asked_questions(self, asked_ids: List[int]) -> List[dict]:
        """Gets multiple questions.

        :param asked_ids: question ids as integer array
        :return: list of dictionaries of the matching questions

        """
        if not asked_ids:
            return []

        cursor = self.db.cursor()
        for_db = str(asked_ids)
        for_db = for_db.replace("[", "")
        for_db = for_db.replace("]", "")
        sql = """
                      SELECT *
                      FROM AskedQuestion q
                      JOIN AskedJson j
                      ON q.asked_json_id = j.asked_json_id
                      WHERE q.asked_id IN (""" + for_db + """)
                      ORDER BY q.asked_id
                      """
        cursor.execute(sql)

        return self.resultAsDictionary(cursor)
