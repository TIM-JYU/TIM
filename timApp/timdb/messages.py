__author__ = 'hajoviin'
from contracts import contract
from timdb.timdbbase import TimDbBase

# TODO: Also need to save the lecture_id to comfirm that we can only show specific question.


class Messages(TimDbBase):


    @contract
    def get_message(self,msg_id: 'int'):
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT user_id,msg_id, message, timestamp
                      FROM Message
                      WHERE msg_id = ?
                      """, [msg_id])

        return self.resultAsDictionary(cursor)

    @contract
    def get_messages(self) -> 'list(dict)':
        """
        Gets the question
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT msg_id,user_id, message, timestamp
                      FROM Message
                      """)

        return self.resultAsDictionary(cursor)

    @contract
    def add_message(self,user_id: 'int', message: 'str', timestamp: 'str', commit: 'bool'=True) -> 'int':
        """ Creates a new message
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO Message (user_id,message,timestamp)
                       VALUES(?,?,?)
                       """, [user_id, message, timestamp])
        if commit:
            self.db.commit()
        msg_id = cursor.lastrowid
        return msg_id