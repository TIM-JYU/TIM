""""""
from timdb.timdbbase import TimDbBase
from contracts import contract


class Answers(TimDbBase):
    @contract
    def saveAnswer(self, user_ids: 'list(int)', task_id: 'str', content: 'str', points: 'str|int|float|None', tags: 'list(str)'):
        """Saves an answer to the database.
        
        :param user_ids: The id of the usergroup to which the answer belongs.
        :param task_id: The id of the task.
        :param content: The content of the answer.
        :param points: Points for the task.
        """

        cursor = self.db.cursor()

        existing_answers = self.get_common_answers(user_ids, task_id)
        if len(existing_answers) > 0 and existing_answers[0]['content'] == content:
            cursor.execute("""UPDATE Answer SET points = ? WHERE id = ?""",
                           [points, existing_answers[0]['id']])
            self.db.commit()
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
        cursor.execute("""SELECT Answer.id, task_id, content, points, datetime(answered_on, 'localtime') as answered_on
                          FROM Answer
                          JOIN UserAnswer ON Answer.id = UserAnswer.answer_id
                          WHERE task_id = ?
                            AND user_id = ?
                          ORDER BY answered_on DESC""", [task_id, user_id])

        answers = self.resultAsDictionary(cursor)
        for answer in answers:
            cursor.execute("""SELECT user_id, real_name FROM UserAnswer
                              JOIN Answer ON Answer.id = UserAnswer.answer_id
                              JOIN User ON UserAnswer.user_id = User.id
                              WHERE answer_id = ?""", [answer['id']])
            r = self.resultAsDictionary(cursor)
            answer['collaborators'] = r
        return answers

    @contract
    def get_newest_answers(self, user_id: 'int', task_ids: 'list(str)') -> 'list(dict)':
        template = ','.join('?' * len(task_ids))
        return self.resultAsDictionary(self.db.execute("""SELECT Answer.id, task_id, content, points,
                                  datetime(MAX(answered_on), 'localtime') as answered_on
                           FROM Answer
                           JOIN UserAnswer ON Answer.id = UserAnswer.answer_id
                           WHERE task_id IN (%s)
                            AND user_id = ?
                           GROUP BY task_id""" % template, task_ids + [user_id]))


    @contract
    def get_common_answers(self, user_ids: 'list(int)', task_id: 'str') -> 'list(dict)':
        common_answers_ids = None
        for user_id in user_ids:
            ids = self.db.execute("""SELECT answer_id
                               FROM UserAnswer
                               JOIN Answer ON Answer.id = UserAnswer.answer_id
                               WHERE user_id = ? AND task_id = ?
                            """, [user_id, task_id]).fetchall()
            if common_answers_ids is None:
                common_answers_ids = set()
                for answer_id in ids:
                    common_answers_ids.add(answer_id[0])
            else:
                curr_answers = set()
                for answer_id in ids:
                    curr_answers.add(answer_id[0])
                common_answers_ids.intersection_update(curr_answers)
        if common_answers_ids is None:
            return []
        template = ','.join('?' * len(common_answers_ids))
        common_answers = self.resultAsDictionary(
            self.db.execute("""SELECT id, task_id, content, points
                               FROM Answer
                               WHERE id IN (%s)
                               ORDER BY answered_on DESC
                            """ % template, list(common_answers_ids)))
        return common_answers

    @contract
    def getUsersForTasks(self, task_ids: 'list(str)') -> 'list(dict)':
        cursor = self.db.cursor()
        placeholder = '?'
        placeholders = ', '.join(placeholder for unused in task_ids)
        cursor.execute(
            """
                SELECT User.id, name, real_name, COUNT(DISTINCT task_id) AS task_count, SUM(points) as total_points
                FROM User
                JOIN UserAnswer ON User.id = UserAnswer.user_id
                JOIN Answer ON Answer.id = UserAnswer.answer_id
                JOIN (SELECT Answer.id, MIN(answered_on)
                      FROM Answer
                      JOIN UserAnswer ON UserAnswer.answer_id = Answer.id
                      GROUP BY UserAnswer.user_id, Answer.task_id
                      )tmp ON tmp.id = Answer.id
                WHERE task_id IN (%s)
                GROUP BY User.id
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

    @contract
    def get_users(self, answer_id: 'int') -> 'list(int)':
        """Gets the user ids of the specified answer.

        :param answer_id: The id of the answer.
        :return: The user ids.
        """
        return [u['user_id'] for u in self.resultAsDictionary(
            self.db.execute("""SELECT user_id FROM UserAnswer
                               WHERE answer_id = ?""", [answer_id]))]

    @contract
    def get_task_id(self, answer_id: 'int') -> 'str|None':
        result = self.resultAsDictionary(
                 self.db.execute("""SELECT task_id FROM Answer
                                    WHERE id = ?""", [answer_id]))
        return result[0]['task_id'] if len(result) > 0 else None

    def get_users_by_taskid(self, task_id: 'str'):
        result = self.resultAsDictionary(self.db.execute("""SELECT DISTINCT User.id, name, real_name
                           FROM User
                           JOIN UserAnswer ON UserAnswer.user_id = User.id
                           JOIN Answer on Answer.id = UserAnswer.answer_id
                           WHERE task_id = ?
                           ORDER BY real_name ASC""", [task_id]))
        return result
