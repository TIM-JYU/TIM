__author__ = 'localadmin'
from contracts import contract
from timdb.timdbbase import TimDbBase


class Questions(TimDbBase):
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
    def add_questions(self, question: 'str', answer: 'str', commit: 'bool'=True) -> 'int':
        """ Creates a new questions
        :param question: Question to be saved
        :param answer: Answer to the question
        :param commit: Commit or not to commit
        :return: The id of the newly creater question
        """

        cursor = self.db.cursor()
        cursor.execute('INSERT INTO Question (question,answer) VALUES(?,?)', [question, answer])
        if commit:
            self.db.commit()
        question_id = cursor.lastrowid
        return question_id