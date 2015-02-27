""""""
from timdb.timdbbase import TimDbBase
from contracts import contract


class Answers(TimDbBase):
    @contract
    def saveAnswer(self, user_ids: 'list(int)', task_id: 'str', content: 'str', points: 'str|None', tags: 'list(str)'):
        """Saves an answer to the database.
        
        :param user_ids: The id of the usergroup to which the answer belongs.
        :param task_id: The id of the task.
        :param content: The content of the answer.
        :param points: Points for the task.
        """

        cursor = self.db.cursor()

        if len(user_ids) == 1:
            existing_answers = self.getAnswers(user_ids[0], task_id)
            if len(existing_answers) > 0 and existing_answers[0]['content'] == content:
                return

        cursor.execute('INSERT INTO Answer (task_id, content, points, answered_on) VALUES (?,?,?,CURRENT_TIMESTAMP)',
                       [task_id, content, points])
        answer_id = cursor.lastrowid
        assert answer_id is not None

        for user_id in user_ids:
            cursor.execute('INSERT INTO UserAnswer (user_id, answer_id) VALUES (?,?)', [user_id, answer_id])

        for tag in tags:
            cursor.execute('INSERT INTO AnswerTag (answer_id, tag) VALUES (?,?)', [answer_id, tag])

        self.db.commit()

    @contract
    def getAnswers(self, user_id: 'int', task_id: 'str') -> 'list(dict)':
        """Gets the answers of a user in a task, ordered descending by submission time.
        
        :param user_id: The id of the user.
        :param task_id: The id of the task.
        """

        cursor = self.db.cursor()
        cursor.execute("""SELECT id, task_id, content, points, answered_on FROM Answer WHERE task_id = ?
                          AND id IN
                              (SELECT answer_id FROM UserAnswer WHERE user_id = ?)
                          ORDER BY answered_on DESC""", [task_id, user_id])

        return self.resultAsDictionary(cursor)

    @contract
    def getUsersForTasks(self, task_ids: 'list(str)') -> 'list(dict)':
        cursor = self.db.cursor()
        placeholder = '?'
        placeholders = ', '.join(placeholder for unused in task_ids)
        cursor.execute(
            """
                SELECT id, name, real_name FROM User
                WHERE id IN (
                    SELECT user_id FROM UserAnswer
                    WHERE answer_id IN (
                        SELECT id FROM Answer WHERE task_id IN (%s)
                    )
                )
                ORDER BY real_name ASC
            """ % placeholders, task_ids)
            
        return self.resultAsDictionary(cursor)

    @contract
    def getAnswersForGroup(self, user_ids: 'list(int)', task_id: 'str') -> 'list(dict)':
        """Gets the answers of the users in a task, ordered descending by submission time.
           All users in the list `user_ids` must be associated with the answer.
        
        """

        cursor = self.db.cursor()
        sql = """select id, task_id, content, points, answered_on from Answer where task_id = ?
                          %s
                          order by answered_on desc""" % (
            " ".join(["and id in (select answer_id from UserAnswer where user_id = %d)" % user_id for user_id in user_ids]))
        print(sql)
        cursor.execute(sql, [task_id])
        return self.resultAsDictionary(cursor)
