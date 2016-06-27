""""""
from collections import defaultdict
from typing import List, Optional, Union

from timdb.tim_models import Answer, UserAnswer, AnswerTag
from timdb.timdbbase import TimDbBase
import json


class Answers(TimDbBase):
    def saveAnswer(self,
                   user_ids: List[int],
                   task_id: str,
                   content: str,
                   points: Union[str, int, float, None],
                   tags: List[str],
                   valid: bool=True):
        """
        Saves an answer to the database.
        
        :param tags: Tags for the answer.
        :param valid: Whether the answer is considered valid (e.g. sent before deadline, etc.)
        :param user_ids: The id of the usergroup to which the answer belongs.
        :param task_id: The id of the task.
        :param content: The content of the answer.
        :param points: Points for the task.
        """

        cursor = self.db.cursor()

        existing_answers = self.get_common_answers(user_ids, task_id)
        if len(existing_answers) > 0 and existing_answers[0]['content'] == content:
            existing_id = existing_answers[0]['id']
            a = Answer.query.filter(Answer.id == existing_id).one()
            a.points = points
            self.session.commit()
            return False, existing_id

        a = Answer(task_id=task_id, content=content, points=points, valid=valid)
        self.session.add(a)
        self.session.flush()
        answer_id = a.id
        assert answer_id != 0

        for user_id in user_ids:
            ua = UserAnswer(user_id=user_id, answer_id=answer_id)
            self.session.add(ua)

        for tag in tags:
            at = AnswerTag(answer_id=answer_id, tag=tag)
            self.session.add(at)

        self.session.commit()
        return True, answer_id

    def get_answers(self, user_id: int, task_id: str, get_collaborators: bool=True) -> List[dict]:
        """Gets the answers of a user in a task, ordered descending by submission time.
        
        :param get_collaborators: Whether collaborators for each answer should be fetched.
        :param user_id: The id of the user.
        :param task_id: The id of the task.
        """

        cursor = self.db.cursor()
        cursor.execute("""SELECT Answer.id, task_id, content, points,
                                 answered_on, valid
                          FROM Answer
                          JOIN UserAnswer ON Answer.id = UserAnswer.answer_id
                          WHERE task_id = %s
                            AND user_id = %s
                          ORDER BY answered_on DESC, Answer.id DESC""", [task_id, user_id])

        answers = self.resultAsDictionary(cursor)
        if not get_collaborators:
            return answers

        self.set_collaborators(answers)
        return answers

    def set_collaborators(self, answers):
        if not answers:
            return
        answer_dict = defaultdict(list)
        c = self.db.cursor()
        c.execute("""SELECT answer_id, user_id, real_name FROM UserAnswer
            JOIN Answer ON Answer.id = UserAnswer.answer_id
            JOIN UserAccount ON UserAnswer.user_id = UserAccount.id
            WHERE answer_id IN ({})""".format(','.join(['%s'] * len(answers))),
                  [answer['id'] for answer in answers])
        for row in c.fetchall():
            answer_dict[row[0]].append({'user_id': row[1], 'real_name': row[2]})
        for answer in answers:
            answer['collaborators'] = answer_dict[answer['id']]


    def get_newest_answers(self, user_id: int, task_ids: List[str]) -> List[dict]:
        if len(task_ids) == 0:
            return []
        template = ','.join(['%s'] * len(task_ids))
        c = self.db.cursor()
        c.execute("""
        SELECT task_id, content, points, answered_on
        FROM
        (
        SELECT MAX(Answer.id) as aid
        FROM Answer
        JOIN UserAnswer ON Answer.id = UserAnswer.answer_id
        WHERE task_id IN ({})
         AND user_id = %s
        GROUP BY task_id
        ) t JOIN Answer a ON a.id = t.aid""".format(template), task_ids + [user_id])
        return self.resultAsDictionary(c)

    def get_all_answers(self, task_id: str, usergroup: int, hide_names: bool, age: str) -> List[str]:
        """Gets the all answers to task

        :param task_id: The id of the task.
        :param usergroup: Group of users to search
        :param hide_names: Hide names
        :param age: min or max
        """
        time_limit = "1900-09-12 22:00:00"
        minmax = "max"
        if age == "min":
            minmax = "min"
        c = self.db.cursor()
        c.execute("""
SELECT u.name, a.task_id, a.content, a.answered_on, n
FROM
(SELECT {}(a.id) AS id, COUNT(a.id) as n
FROM answer AS a, userAnswer AS ua, useraccount AS u
WHERE a.task_id LIKE %s AND ua.answer_id = a.id AND u.id = ua.user_id AND a.answered_on > %s
GROUP BY a.task_id, u.id
ORDER BY u.id,a.task_id) t
JOIN answer a ON a.id = t.id JOIN useranswer ua ON ua.answer_id = a.id JOIN useraccount u ON u.id = ua.user_id
        """.format(minmax), [task_id, time_limit])

        result = []

        for row in c.fetchall():
            header = row[0] + ": " + row[1] + "; " + row[3] + "; " + str(row[4])
            if hide_names: header = ""
            # print(separator + header)
            line = json.loads(row[2])
            if not isinstance(line, list):
                answ = line.get("usercode", "-")
                result.append(header + "\n" + answ)
        return result


    def get_common_answers(self, user_ids: List[int], task_id: str) -> List[dict]:
        common_answers_ids = None
        for user_id in user_ids:
            c = self.db.cursor()
            c.execute("""SELECT answer_id
                FROM UserAnswer
                JOIN Answer ON Answer.id = UserAnswer.answer_id
                WHERE user_id = %s AND task_id = %s
             """, [user_id, task_id])
            ids = c.fetchall()
            if common_answers_ids is None:
                common_answers_ids = set()
                for answer_id in ids:
                    common_answers_ids.add(answer_id[0])
            else:
                curr_answers = set()
                for answer_id in ids:
                    curr_answers.add(answer_id[0])
                common_answers_ids.intersection_update(curr_answers)
        if common_answers_ids is None or len(common_answers_ids) == 0:
            return []
        template = ','.join(['%s'] * len(common_answers_ids))
        c = self.db.cursor()
        c.execute("""SELECT id, task_id, content, points
            FROM Answer
            WHERE id IN ({})
            ORDER BY answered_on DESC, id DESC
         """.format(template), list(common_answers_ids))
        common_answers = self.resultAsDictionary(c)
        return common_answers

    def getUsersForTasks(self, task_ids: List[str], user_ids: Optional[List[int]]=None) -> List[dict]:
        if not task_ids:
            return []
        cursor = self.db.cursor()
        task_id_template = ','.join(['%s']*len(task_ids))
        user_restrict_sql = '' if user_ids is None else 'AND UserAccount.id IN ({})'.format(','.join(['%s']*len(user_ids)))
        if user_ids is None:
            user_ids = []
        cursor.execute(
            """
                SELECT UserAccount.id, name, real_name, COUNT(DISTINCT task_id) AS task_count, ROUND(SUM(cast(points as float))::numeric,2) as total_points
                FROM UserAccount
                JOIN UserAnswer ON UserAccount.id = UserAnswer.user_id
                JOIN (

                      (SELECT Answer.task_id, UserAnswer.user_id as uid, MAX(Answer.id) as aid
                      FROM Answer
                      JOIN UserAnswer ON UserAnswer.answer_id = Answer.id
                      WHERE task_id IN ({}) AND Answer.valid = TRUE
                      GROUP BY UserAnswer.user_id, Answer.task_id) a1
                      JOIN (SELECT id, points FROM Answer) a2 ON a2.id = a1.aid

                      ) tmp ON tmp.aid = UserAnswer.answer_id AND UserAccount.id = tmp.uid
                {}
                GROUP BY UserAccount.id
                ORDER BY real_name ASC
            """.format(task_id_template, user_restrict_sql), task_ids + user_ids)
            
        return self.resultAsDictionary(cursor)

    def getAnswersForGroup(self, user_ids: List[int], task_id: str) -> List[dict]:
        """Gets the answers of the users in a task, ordered descending by submission time.
           All users in the list `user_ids` must be associated with the answer.
        
        """

        cursor = self.db.cursor()
        sql = """select id, task_id, content, points, answered_on from Answer where task_id = %s
                          %s
                          order by answered_on desc""" % (
            " ".join(["and id in (select answer_id from UserAnswer where user_id = %d)" % user_id for user_id in user_ids]))
        cursor.execute(sql, [task_id])
        return self.resultAsDictionary(cursor)

    def get_users(self, answer_id: int) -> List[int]:
        """Gets the user ids of the specified answer.

        :param answer_id: The id of the answer.
        :return: The user ids.
        """
        c = self.db.cursor()
        c.execute("""SELECT user_id FROM UserAnswer
            WHERE answer_id = %s""", [answer_id])
        return [u['user_id'] for u in self.resultAsDictionary(c)]

    def get_task_id(self, answer_id: int) -> Optional[str]:
        c = self.db.cursor()
        c.execute("""SELECT task_id FROM Answer
            WHERE id = %s""", [answer_id])
        result = self.resultAsDictionary(c)
        return result[0]['task_id'] if len(result) > 0 else None


    def get_users_by_taskid(self, task_id: str):
        c = self.db.cursor()
        c.execute("""SELECT DISTINCT UserAccount.id, name, real_name
            FROM UserAccount
            JOIN UserAnswer ON UserAnswer.user_id = UserAccount.id
            JOIN Answer on Answer.id = UserAnswer.answer_id
            WHERE task_id = %s
            ORDER BY real_name ASC""", [task_id])
        result = self.resultAsDictionary(c)
        return result

    def get_answer(self, answer_id: int) -> Optional[dict]:
        cursor = self.db.cursor()
        cursor.execute("""SELECT id, task_id, content, points,
                                 answered_on, valid
                          FROM Answer
                          WHERE id = %s
                          ORDER BY answered_on DESC""", [answer_id])

        answers = self.resultAsDictionary(cursor)
        self.set_collaborators(answers)
        return answers[0] if len(answers) > 0 else None
