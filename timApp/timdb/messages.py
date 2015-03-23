__author__ = 'localadmin'
from contracts import contract
from timdb.timdbbase import TimDbBase


class Messages(TimDbBase):


    @contract
    def get_message(self,msg_id: 'int'):
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT msg_id, message, timestamp
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
        cursor.execute("""SELECT msg_id, message, timestamp FROM Message """)

        return self.resultAsDictionary(cursor)

    @contract
    def add_message(self, message: 'str', timestamp: 'str', commit: 'bool'=True) -> 'int':
        """ Creates a new message
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO Message (message,timestamp)
                       VALUES(?,?)
                       """, [message, timestamp])
        if commit:
            self.db.commit()
        msg_id = cursor.lastrowid
        return msg_id