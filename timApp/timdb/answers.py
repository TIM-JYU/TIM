""""""
import json
import re
from collections import defaultdict, OrderedDict
from datetime import datetime
from operator import itemgetter
from typing import List, Optional, Dict

from documentmodel.pointsumrule import PointSumRule, PointType
from utils import get_sql_template
from timdb.tim_models import Answer, UserAnswer, AnswerTag
from timdb.timdbbase import TimDbBase


class Answers(TimDbBase):
    def save_answer(self,
                    user_ids: List[int],
                    task_id: str,
                    content: str,
                    points: Optional[float],
                    tags: Optional[List[str]]=None,
                    valid: bool=True,
                    points_given_by=None):
        """
        Saves an answer to the database.
        
        :param points_given_by: The usergroup id who gave the points, or None if they were given by a plugin.
        :param tags: Tags for the answer.
        :param valid: Whether the answer is considered valid (e.g. sent before deadline, etc.)
        :param user_ids: The id of the usergroup to which the answer belongs.
        :param task_id: The id of the task.
        :param content: The content of the answer.
        :param points: Points for the task.
        """
        if tags is None:
            tags = []
        existing_answers = self.get_common_answers(user_ids, task_id)
        if len(existing_answers) > 0 and existing_answers[0]['content'] == content:
            existing_id = existing_answers[0]['id']
            a = Answer.query.filter(Answer.id == existing_id).one()
            a.points = points
            a.last_points_modifier = points_given_by
            self.session.commit()
            return None

        a = Answer(task_id=task_id, content=content, points=points, valid=valid, last_points_modifier=points_given_by)
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
        return answer_id

    def get_answers(self, user_id: int, task_id: str, get_collaborators: bool=True) -> List[dict]:
        """Gets the answers of a user in a task, ordered descending by submission time.
        
        :param get_collaborators: Whether collaborators for each answer should be fetched.
        :param user_id: The id of the user.
        :param task_id: The id of the task.
        """

        cursor = self.db.cursor()
        cursor.execute("""SELECT Answer.id, task_id, content, points,
                                 answered_on, valid, last_points_modifier
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
        c.execute("""SELECT answer_id, user_id, real_name, email FROM UserAnswer
            JOIN Answer ON Answer.id = UserAnswer.answer_id
            JOIN UserAccount ON UserAnswer.user_id = UserAccount.id
            WHERE answer_id IN ({})""".format(','.join(['%s'] * len(answers))),
                  [answer['id'] for answer in answers])
        for row in c.fetchall():
            answer_dict[row[0]].append({'user_id': row[1], 'real_name': row[2], 'email': row[3]})
        for answer in answers:
            answer['collaborators'] = answer_dict[answer['id']]

    def get_newest_answers(self, user_id: int, task_ids: List[str]) -> List[dict]:
        if len(task_ids) == 0:
            return []
        template = ','.join(['%s'] * len(task_ids))
        c = self.db.cursor()
        c.execute("""
        SELECT task_id, content, points, answered_on, valid, cnt
        FROM
        (
        SELECT MAX(Answer.id) as aid, COUNT(Answer.id) as cnt
        FROM Answer
        JOIN UserAnswer ON Answer.id = UserAnswer.answer_id
        WHERE task_id IN ({})
         AND user_id = %s
        GROUP BY task_id
        ) t JOIN Answer a ON a.id = t.aid""".format(template), task_ids + [user_id])
        return self.resultAsDictionary(c)

    def get_all_answers(self,
                        task_ids: List[str],
                        usergroup: Optional[int],
                        hide_names: bool,
                        age: str,
                        valid: str,
                        printname:bool,
                        sort: str,
                        print_opt: str,
                        period_from: datetime,
                        period_to: datetime) -> List[str]:
        """Gets all answers to the specified tasks.

        :param period_from: The minimum answering time for answers.
        :param period_to: The maximum answering time for answers.
        :param sort: Sorting order for answers.
        :param task_ids: The ids of the tasks.
        :param usergroup: Group of users to search
        :param hide_names: Hide names
        :param age: min, max or all
        :param valid: 0, 1 or all
        :param printname: True = put user full name as first in every task
        :param print_opt: all = header and answers, header=only header, answers=only answers, korppi=korppi form
        """
        counts =  "count(a.answered_on)"
        groups = "group by a.task_id, u.id"
        print_header = print_opt == "all" or print_opt == "header"
        print_answers = print_opt == "all" or print_opt == "answers"

        minmax = "max"
        if age == "min":
            minmax = "min"
        if age == "all":
            minmax = ""
            counts = "a.valid"
            groups = ""

        validstr = "AND a.valid"
        if valid == "all":
            validstr = ""
        if valid == "0":
            validstr = "AND NOT a.valid"

        order_by = 'a.task_id, u.name'
        if sort == 'username':
            order_by = 'u.name, a.task_id'

        c = self.db.cursor()
        sql = """
SELECT DISTINCT u.name, a.task_id, a.content, a.answered_on, n, a.points, u.real_name
FROM
(SELECT {} (a.id) AS id, {} as n
FROM answer AS a, userAnswer AS ua, useraccount AS u
WHERE a.task_id IN ({}) AND ua.answer_id = a.id AND u.id = ua.user_id AND a.answered_on >= %s AND a.answered_on < %s
{}
{}
) t
JOIN answer a ON a.id = t.id JOIN useranswer ua ON ua.answer_id = a.id JOIN useraccount u ON u.id = ua.user_id
ORDER BY {}, a.answered_on
        """
        sql = sql.format(minmax, counts, get_sql_template(task_ids), validstr, groups, order_by)
        c.execute(sql, task_ids + [period_from, period_to])

        result = []

        cnt = 0
        for row in c.fetchall():
            cnt += 1
            points = str(row[5])
            if points == "None": points = ""
            n = str(int(row[4]))
            name = row[0]
            if hide_names: name = "user" + str(cnt)
            header = name + "; " + row[1] + "; " + str(row[3]) + "; " + n + "; " + points
            # print(separator + header)
            line = json.loads(row[2])
            answ = str(line)
            if isinstance(line, dict):  # maybe csPlugin?
                if "usercode" in line:  # is csPlugin
                    answ = line.get("usercode", "-")
                else:
                    if "points" in line:    # empty csPlugin answer
                        answ = ""

            res = ""
            if printname and not hide_names: header = str(row[6]) + "; " + header
            if print_header: res = header
            if print_answers: res += "\n" + answ
            if print_opt == "korppi":
                res = name + ";"
                taskid = row[1]
                i = taskid.find(".")
                if i >= 0:
                    taskid = taskid[i+1:]
                res += taskid + ";" + answ.replace("\n", "\\n")

            result.append(res)
        return result


    def check_if_plugin_has_answers(self, task_id: 'str') -> 'int':
        """
        Checks if there are answers to the plugin
        :param task_id: The task id of plugin ín format doc_id.par_id
        :return: 0 if not found, 1 if answers exist
        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT EXISTS(SELECT 1 FROM Answer WHERE task_id = %s LIMIT 1)""", [task_id])
        result = cursor.fetchone()
        real_result = result[0]
        return real_result


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

    def get_users_for_tasks(self, task_ids: List[str], user_ids: Optional[List[int]]=None, group_by_user=True) -> List[Dict[str, str]]:
        if not task_ids:
            return []
        cursor = self.db.cursor()
        task_id_template = ','.join(['%s']*len(task_ids))
        user_restrict_sql = '' if user_ids is None else 'AND UserAccount.id IN ({})'.format(','.join(['%s']*len(user_ids)))
        if user_ids is None:
            user_ids = []
        sql = """
                SELECT *, task_points + COALESCE(velp_points, 0) as total_points
                FROM (
                SELECT UserAccount.id, name, real_name, email,
                       COUNT(task_id) AS task_count,
                       ROUND(SUM(cast(points as float))::numeric,2) as task_points,
                       ROUND(SUM(velp_points)::numeric,2) as velp_points,
                       COUNT(annotation_answer_id) AS velped_task_count
                       {}
                FROM UserAccount
                JOIN UserAnswer ON UserAccount.id = UserAnswer.user_id
                JOIN (

                      (SELECT Answer.task_id, UserAnswer.user_id as uid, MAX(Answer.id) as aid
                      FROM Answer
                      JOIN UserAnswer ON UserAnswer.answer_id = Answer.id
                      WHERE task_id IN ({}) AND Answer.valid = TRUE
                      GROUP BY UserAnswer.user_id, Answer.task_id) a1
                      JOIN (SELECT id, points FROM Answer) a2 ON a2.id = a1.aid
                      LEFT JOIN (SELECT
                                 answer_id as annotation_answer_id,
                                 SUM(points) as velp_points
                                 FROM annotation
                                 WHERE valid_until IS NULL
                                 GROUP BY answer_id
                                 ) a3 ON a3.annotation_answer_id = a1.aid

                      ) tmp ON tmp.aid = UserAnswer.answer_id AND UserAccount.id = tmp.uid
                {}
                GROUP BY UserAccount.id {}
                ORDER BY real_name ASC
                ) tmp
            """.format(', MIN(task_id) as task_id' if not group_by_user else '',
                       task_id_template,
                       user_restrict_sql,
                       '' if group_by_user else ', task_id')
        cursor.execute(sql, task_ids + user_ids)
        return self.resultAsDictionary(cursor)

    def get_points_by_rule(self, points_rule: Optional[Dict],
                           task_ids: List[str],
                           user_ids: Optional[List[int]]=None,
                           flatten: bool=False):
        """Computes the point sum from given tasks accoring to the given point rule.
        :param points_rule: The points rule.
        :param task_ids: The list of task ids to consider.
        :param user_ids: The list of users for which to compute the sum.
        :param flatten: Whether to return the result as a list of dicts (True) or as a deep dict (False).
        :return: The computed result.
        """
        if not points_rule:
            return self.get_users_for_tasks(task_ids, user_ids)
        tasks_users = self.get_users_for_tasks(task_ids, user_ids, group_by_user=False)
        rule = PointSumRule(points_rule)
        result = defaultdict(lambda:defaultdict(lambda:defaultdict(lambda:defaultdict(list))))
        for tu in tasks_users:
            for group in rule.find_groups(tu['task_id']):
                result[tu['id']]['groups'][group]['tasks'].append(tu)
        for user_id, task_groups in result.items():
            groups = task_groups['groups']
            groupsums = []
            for groupname, group in groups.items():
                group['task_sum'] = 0
                group['velp_sum'] = 0
                if PointType.task in rule.groups[groupname].point_types:
                    group['task_sum'] = round(sum(t['task_points'] for t in group['tasks'] if t['task_points'] is not None), 2)
                if PointType.velp in rule.groups[groupname].point_types:
                    group['velp_sum'] = round(sum(t['velp_points'] for t in group['tasks'] if t['velp_points'] is not None), 2)
                group['velped_task_count'] = sum(1 for t in group['tasks'] if t['velped_task_count'] > 0)
                group['total_sum'] = group['task_sum'] + group['velp_sum']
                groupsums.append((group['task_sum'], group['velp_sum'], group['total_sum']))
            if rule.count_type == 'best':
                groupsums = sorted(groupsums, reverse=True, key=itemgetter(2))
            else:
                groupsums = sorted(groupsums, key=itemgetter(2))
            try:
                task_groups['task_sum'] = round(sum(s[0] for s in groupsums[0:rule.count_amount]), 2)
                task_groups['velp_sum'] = round(sum(s[1] for s in groupsums[0:rule.count_amount]), 2)
                task_groups['total_sum'] = round(sum(s[2] for s in groupsums[0:rule.count_amount]), 2)
            except TypeError:
                task_groups['task_sum'] = 0
                task_groups['velp_sum'] = 0
                task_groups['total_sum'] = 0
        if flatten:
            result_list = []
            for user_id, task_groups in result.items():
                first_group = next(v for _, v in task_groups['groups'].items())
                row = first_group['tasks'][0]
                row['total_points'] = task_groups['total_sum']
                row['task_points'] = task_groups['task_sum']
                row['velp_points'] = task_groups['velp_sum']
                row['task_count'] = len(task_groups['groups'])
                row['velped_task_count'] = sum(1 for t in task_groups['groups'].values() if t['velped_task_count'] > 0)
                row.pop('task_id', None)
                row['groups'] = OrderedDict()
                for groupname, _ in sorted(rule.groups.items()):
                    row['groups'][groupname] = {'task_sum': task_groups['groups'].get(groupname, {}).get('task_sum', 0),
                                                'velp_sum': task_groups['groups'].get(groupname, {}).get('velp_sum', 0),
                                                'total_sum': task_groups['groups'].get(groupname, {}).get('total_sum', 0)}
                result_list.append(row)
            return result_list
        return result

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
        c.execute("""SELECT DISTINCT UserAccount.id, name, real_name, email
            FROM UserAccount
            JOIN UserAnswer ON UserAnswer.user_id = UserAccount.id
            JOIN Answer on Answer.id = UserAnswer.answer_id
            WHERE task_id = %s
            ORDER BY real_name ASC""", [task_id])
        result = self.resultAsDictionary(c)
        return result

    def get_answer(self, answer_id: int) -> Optional[dict]:
        """Gets data for a single answer.
        The field 'cnt' is the 1-based index of the answer for the user (or the set of collaborators) for the task.
        So cnt for the first answer is always 1.
        """
        cursor = self.db.cursor()
        cursor.execute("""SELECT id, task_id, content, points,
                                 answered_on, valid, cnt
                          FROM Answer a
                          CROSS JOIN LATERAL (
                            SELECT COUNT(*) as cnt
                            FROM Answer b
                            JOIN useranswer ua ON ua.answer_id = b.id
                            WHERE b.task_id = a.task_id
                              AND b.answered_on <= a.answered_on
                              AND ua.user_id IN (SELECT user_id FROM useranswer WHERE answer_id = a.id)
                          ) t
                          WHERE id = %s
                          ORDER BY answered_on DESC""", [answer_id])

        answers = self.resultAsDictionary(cursor)
        self.set_collaborators(answers)
        return answers[0] if len(answers) > 0 else None
