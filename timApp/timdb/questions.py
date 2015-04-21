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
                          SELECT id, question, answer
                          FROM Question
                          WHERE doc_id = ? AND par_index = ?
                       """, [doc_id, par_index])
        return self.resultAsDictionary(cursor)

    @contract
    def delete_question(self, question_id: 'int'):
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

        self.db.commit()

    @contract
    def get_question(self, question_id) -> 'list(dict)':
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
            """, [question_id]
        )

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
    def add_questions(self, doc_id: 'int', par_index:'int', question: 'str', answer: 'str', questionJson: 'str',
                      commit: 'bool'=True) -> 'int':
        """ Creates a new questions
        :param question: Question to be saved
        :param answer: Answer to the question
        :param commit: Commit or not to commit
        :return: The id of the newly creater question
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO Question (doc_id, par_index, question,answer, questionJson)
                       VALUES(?,?,?,?,?)
                       """, [doc_id, par_index, question, answer, questionJson])
        if commit:
            self.db.commit()
        question_id = cursor.lastrowid
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
