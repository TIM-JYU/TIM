__author__ = 'hajoviin'
from contracts import contract
from timdb.timdbbase import TimDbBase

# TODO: Also need to save the lecture_id to comfirm that we can only show specific question.


class Messages(TimDbBase):
    @contract
    def delete_messages_from_lecture(self, lecture_id:"int", commit: "bool"):
        cursor = self.db.cursor()
        cursor.execute(
            """
            DELETE FROM Message
            WHERE lecture_id = ?
            """, [lecture_id]
        )

        if commit:
            self.db.commit()

    @contract
    def get_message(self, msg_id: 'int'):
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT user_id,msg_id, message, timestamp
                      FROM Message
                      WHERE msg_id = ?
                      """, [msg_id])

        return self.resultAsDictionary(cursor)

    @contract
    def get_messages(self, lecture_id: 'int') -> 'list(dict)':
        """
        Gets the question
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT msg_id, user_id, message, timestamp
                      FROM Message
                      WHERE lecture_id = ?
                      """, [lecture_id]
        )

        return self.resultAsDictionary(cursor)

    @contract
    def get_messages_amount(self, lecture_id: 'int', amount: 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT *
                      FROM Message
                      WHERE lecture_id = (?)
                      ORDER BY msg_id
                      DESC
                      LIMIT (?)
                      """, [lecture_id, amount])

        return self.resultAsDictionary(cursor)

    def get_last_message(self, lecture_id: "int") -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT *
                      FROM Message
                      WHERE lecture_id = (?)
                      ORDER BY msg_id
                      DESC
                      LIMIT 1
                      """, [lecture_id])

        return self.resultAsDictionary(cursor)

    @contract
    def add_message(self, user_id: 'int', lecture_id: 'int', message: 'str', timestamp: 'str',
                    commit: 'bool'=True) -> 'int':
        """ Creates a new message
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO
                       Message(user_id,lecture_id,message,timestamp)
                       VALUES(?,?,?,?)
                       """, [user_id, lecture_id, message, timestamp])
        if commit:
            self.db.commit()
        msg_id = cursor.lastrowid
        return msg_id