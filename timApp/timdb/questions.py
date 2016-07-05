from datetime import datetime
from typing import List, Optional

from timdb.tim_models import AskedQuestion, AskedJson, Question

__author__ = 'hajoviin'
from timdb.timdbbase import TimDbBase


class Questions(TimDbBase):
    # TODO: Doesn't work until question table has been altered
    def get_paragraphs_question(self, doc_id: int, par_index: int):
        """
        Gets the questions of some paragraph
        :param par_id: Paragraph to get question.
        :return: The list of question from that paragraph
        """

        cursor = self.db.cursor()
        cursor.execute("""
                          SELECT question_id, question, answer
                          FROM Question
                          WHERE doc_id = %s AND par_id = %s
                       """, [doc_id, par_index])
        return self.resultAsDictionary(cursor)

    def delete_question(self, question_id: int, commit:bool=True):
        """
        Deletes the question from database
        :param question_id: question to delete
        """

        cursor = self.db.cursor()

        cursor.execute(
            """
                DELETE FROM Question
                WHERE question_id = %s
            """, [question_id])

        if commit:
            self.db.commit()

    def get_question(self, question_id: int) -> List[dict]:
        """
        Gets question with specific id
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


    def get_questions(self) -> List[dict]:
        """
        Gets the question
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT id, question, answer FROM Question """)

        return self.resultAsDictionary(cursor)

    def add_asked_questions(self, lecture_id: int, doc_id: int, par_id: Optional[str], asked_time: datetime,
                            points: str, asked_json_id: int, expl: str, commit: bool=True) -> int:

        """
        Creates a new asked questions
        :param lecture_id: Lecture where question was asked
        :param doc_id: Document, where the question belongs
        :param par_id: Paragraph, where the question belongs
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

    def update_asked_question_points(self, asked_id: int, points: str, commit: bool=True) -> int:

        """
        Creates a new asked questions
        :param commit: Commit or not to commit
        :return: The id of the newly created asked question
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       UPDATE AskedQuestion
                       SET points = %s
                       WHERE asked_id = %s
                       """, [points, asked_id])
        if commit:
            self.db.commit()
        return asked_id

    def get_asked_question(self, asked_id: int) -> List[dict]:
        """
        Gets the asked question by id
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute(
            """
            SELECT *
            FROM AskedQuestion a
            INNER JOIN AskedJson j
            WHERE a.asked_id = %s AND a.asked_json_id == j.asked_json_id
            """, [asked_id])

        return self.resultAsDictionary(cursor)

    def add_asked_json(self, json: str, hash: str, commit: bool=True) -> int:
        """
        Gets the asked question by id
        :return: Questions as a list
        """
        aj = AskedJson(json=json, hash=hash)
        self.session.add(aj)
        self.session.flush()
        if commit:
            self.session.commit()
        return aj.asked_json_id

    def get_asked_json_by_id(self, asked_json_id: int) -> List[dict]:
        """
        Gets the asked question by id
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
        """
        Gets the asked question by id
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

    def add_questions(self, doc_id: int, par_id: str, question_title: str, answer: str, questionJson: str,
                      points: str, expl: str, commit: bool=True) -> int:
        """
        Creates a new questions
        :param question_title: Question to be saved
        :param answer: Answer to the question
        :param commit: Commit or not to commit
        :return: The id of the newly creater question
        """

        q = Question(doc_id=doc_id, par_id=par_id, question_title=question_title, answer=answer,
                     questionjson=questionJson, points=points, expl=expl)
        self.session.add(q)
        self.session.flush()
        if commit:
            self.session.commit()
        return q.question_id


    def update_question(self, question_id: int, doc_id: int, par_id: str, question_title: str, answer: str,
                        questionJson: str, points: str, expl: str,) -> int:
        """
        Updates the question with particular id
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       UPDATE Question
                       SET doc_id = %s, par_id = %s, question_title = %s, answer = %s, questionJson = %s, points = %s, expl = %s
                       WHERE question_id = %s
                       """, [doc_id, par_id, question_title, answer, questionJson, points, expl, question_id])

        self.db.commit()
        return question_id

    def get_doc_questions(self, doc_id: int) -> List[dict]:
        """
        Gets questions related to a specific document
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT *
                      FROM Question
                      WHERE doc_id = %s
                      """, [doc_id])

        return self.resultAsDictionary(cursor)

    def get_multiple_questions(self, question_ids: 'int[]') -> List[dict]:
        """
        Gets multiple questions
        :param question_ids: quesitons ids as integet array
        :return: list of dictionaries of the matching questions.
        """

        cursor = self.db.cursor()
        for_db = str(question_ids)
        for_db = for_db.replace("[", "")
        for_db = for_db.replace("]", "")
        cursor.execute("""
                      SELECT *
                      FROM Question
                      WHERE question_id IN (""" + for_db + """)
                      """)

        return self.resultAsDictionary(cursor)

    def get_multiple_asked_questions(self, asked_ids: 'int[]') -> List[dict]:
        """
        Gets multiple questions
        :param question_ids: quesitons ids as integet array
        :return: list of dictionaries of the matching questions.
        """

        cursor = self.db.cursor()
        for_db = str(asked_ids)
        for_db = for_db.replace("[", "")
        for_db = for_db.replace("]", "")
        cursor.execute("""
                      SELECT *
                      FROM AskedQuestion q
                      INNER JOIN AskedJson j
                      ON q.asked_json_id == j.asked_json_id
                      WHERE q.asked_id IN (""" + for_db + """)
                      """)

        return self.resultAsDictionary(cursor)
