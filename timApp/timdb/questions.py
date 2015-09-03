__author__ = 'hajoviin'
from contracts import contract
from timdb.timdbbase import TimDbBase


class Questions(TimDbBase):
    # TODO: Doesn't work until question table has been altered
    @contract
    def get_paragraphs_question(self, doc_id: 'int', par_index: 'int'):
        """
        Gets the questions of some paragraph
        :param par_id: Paragraph to get question.
        :return: The list of question from that paragraph
        """

        cursor = self.db.cursor()
        cursor.execute("""
                          SELECT question_id, question, answer
                          FROM Question
                          WHERE doc_id = ? AND par_id = ?
                       """, [doc_id, par_index])
        return self.resultAsDictionary(cursor)

    @contract
    def delete_question(self, question_id: 'int', commit:'bool'=True):
        """
        Deletes the question from database
        :param question_id: question to delete
        """

        cursor = self.db.cursor()

        cursor.execute(
            """
                DELETE FROM Question
                WHERE question_id = ?
            """, [question_id])

        if commit:
            self.db.commit()

    @contract
    def get_question(self, question_id: 'int') -> 'list(dict)':
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
            WHERE question_id = ?
            """, [question_id])

        return self.resultAsDictionary(cursor)


    @contract
    def get_questions(self) -> 'list(dict)':
        """
        Gets the question
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT id, question, answer FROM Question """)

        return self.resultAsDictionary(cursor)

    @contract
    def add_asked_questions(self, lecture_id: 'int', doc_id: 'int', par_id: 'str|None', asked_time: 'str',
                            points: 'str', asked_json_id: 'int', expl: 'str', commit: 'bool'=True) -> 'int':

        """
        Creates a new asked questions
        :param lecture_id: Lecture where question was asked
        :param doc_id: Document, where the question belongs
        :param par_id: Paragraph, where the question belongs
        :param asked_json_id: Json of asked question
        :param commit: Commit or not to commit
        :return: The id of the newly created asked question
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO AskedQuestion (lecture_id, doc_id, par_id, asked_time, points, asked_json_id, expl)
                       VALUES(?,?,?,?,?,?,?)
                       """, [lecture_id, doc_id, par_id, asked_time, points, asked_json_id, expl])
        if commit:
            self.db.commit()
        question_id = cursor.lastrowid
        return question_id

    @contract
    def update_asked_question_points(self, asked_id: 'int', points: 'str', commit: 'bool'=True) -> 'int':

        """
        Creates a new asked questions
        :param commit: Commit or not to commit
        :return: The id of the newly created asked question
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       UPDATE AskedQuestion
                       SET points = ?
                       WHERE asked_id = ?
                       """, [points, asked_id])
        if commit:
            self.db.commit()
        return asked_id

    @contract
    def get_asked_question(self, asked_id: 'int') -> 'list(dict)':
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
            WHERE a.asked_id = ? AND a.asked_json_id == j.asked_json_id
            """, [asked_id])

        return self.resultAsDictionary(cursor)

    @contract
    def add_asked_json(self, json: 'str', hash: 'str', commit: 'bool'=True) -> 'int':
        """
        Gets the asked question by id
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute(
            """
            INSERT INTO AskedJson (json, hash)
            VALUES (?, ?)
            """, [json, hash])

        if commit:
            self.db.commit()
        asked_json_id = cursor.lastrowid
        return asked_json_id

    @contract
    def get_asked_json_by_id(self, asked_json_id: 'int') -> 'list(dict)':
        """
        Gets the asked question by id
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute(
            """
            SELECT *
            FROM AskedJson
            WHERE asked_json_id = ?
            """, [asked_json_id])

        return self.resultAsDictionary(cursor)

    @contract
    def get_asked_json_by_hash(self, asked_hash: 'str') -> 'list(dict)':
        """
        Gets the asked question by id
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute(
            """
            SELECT *
            FROM AskedJson
            WHERE hash = ?
            """, [asked_hash])

        return self.resultAsDictionary(cursor)

    @contract
    def add_questions(self, doc_id: 'int', par_id: 'str', question_title: 'str', answer: 'str', questionJson: 'str',
                      points: 'str', expl: 'str', commit: 'bool'=True) -> 'int':
        """
        Creates a new questions
        :param question_title: Question to be saved
        :param answer: Answer to the question
        :param commit: Commit or not to commit
        :return: The id of the newly creater question
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO Question (doc_id, par_id, question_title, answer, questionJson, points, expl)
                       VALUES(?,?,?,?,?,?,?)
                       """, [doc_id, par_id, question_title, answer, questionJson, points, expl])
        if commit:
            self.db.commit()
        question_id = cursor.lastrowid
        return question_id


    @contract
    def update_question(self, question_id: 'int', doc_id: 'int', par_id: 'str', question_title: 'str', answer: 'str',
                        questionJson: 'str', points: 'str', expl: 'str',) -> 'int':
        """
        Updates the question with particular id
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       UPDATE Question
                       SET doc_id = ?, par_id = ?, question_title = ?, answer = ?, questionJson = ?, points = ?, expl = ?
                       WHERE question_id = ?
                       """, [doc_id, par_id, question_title, answer, questionJson, points, expl, question_id])

        self.db.commit()
        return question_id

    @contract
    def get_doc_questions(self, doc_id: 'int') -> 'list(dict)':
        """
        Gets questions related to a specific document
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT *
                      FROM Question
                      WHERE doc_id = ?
                      """, [doc_id])

        return self.resultAsDictionary(cursor)

    def get_multiple_questions(self, question_ids: 'int[]') -> 'list(dict)':
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

    def get_multiple_asked_questions(self, asked_ids: 'int[]') -> 'list(dict)':
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
