__author__ = 'hajoviin'
from contracts import contract
from timdb.timdbbase import TimDbBase

# TODO: Also need to save the lecture_id to comfirm that we can only show specific question.


class Messages(TimDbBase):
    @contract
    def create_message_table(self, lecture_id: 'int', commit: 'bool'):
        message_table = "Message" + str(lecture_id)
        cursor = self.db.cursor()
        cursor.execute("CREATE TABLE " + message_table +
                       "(msg_id INTEGER PRIMARY KEY, " +
                       "user_id INTEGER NOT NULL, " +
                       "lecture_id INTEGER NOT NULL, " +
                       "message TEXT NOT NULL," +
                       "timestamp TEXT NOT NULL" +
                       ")"
        )

        if commit:
            self.db.commit()

        return message_table

    @contract
    def delete_message_table(self, wall_name, commit: 'bool'):
        cursor = self.db.cursor()
        cursor.execute("DROP TABLE " + wall_name)
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
    def get_messages(self, wall_name: 'string') -> 'list(dict)':
        """
        Gets the question
        :return: Questions as a list
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT msg_id, user_id, message, timestamp
                      FROM """ + wall_name
        )

        return self.resultAsDictionary(cursor)

    @contract
    def get_messages_amount(self, wall_name: 'string', amount: 'int') -> 'list(dict)':
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT *
                      FROM """ + wall_name + """
                      ORDER BY msg_id
                      DESC
                      LIMIT (?)
                      """, [amount])

        return self.resultAsDictionary(cursor)

    def get_last_message(self, wall_name: "str") -> 'list(dict)':
        if not wall_name:
            return
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT *
                      FROM """+ wall_name + """
                      ORDER BY msg_id
                      DESC
                      LIMIT 1
                      """)

        return self.resultAsDictionary(cursor)

    @contract
    def add_message(self,wall_name: 'string', user_id: 'int', lecture_id: 'int', message: 'str', timestamp: 'str',
                    commit: 'bool'=True) -> 'int':
        """ Creates a new message
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO """ + wall_name + """ (user_id,lecture_id,message,timestamp)
                       VALUES(?,?,?,?)
                       """, [user_id, lecture_id, message, timestamp])
        if commit:
            self.db.commit()
        msg_id = cursor.lastrowid
        return msg_id