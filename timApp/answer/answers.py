""""""
import json
from collections import defaultdict, OrderedDict
from datetime import datetime
from operator import itemgetter
from typing import List, Optional, Dict, Tuple, Iterable

from sqlalchemy import func

from timApp.answer.answer import Answer
from timApp.answer.answer_models import AnswerTag, UserAnswer
from timApp.answer.pointsumrule import PointSumRule, PointType
from timApp.timdb.sqa import tim_main_execute
from timApp.timdb.timdbbase import TimDbBase, result_as_dict_list
from timApp.user.user import Consent, User


class Answers(TimDbBase):

    def save_answer(self,
                    users: List[User],
                    task_id: str,
                    content: str,
                    points: Optional[float],
                    tags: Optional[List[str]]=None,
                    valid: bool=True,
                    points_given_by=None):
        """Saves an answer to the database.

        :param points_given_by: The usergroup id who gave the points, or None if they were given by a plugin.
        :param tags: Tags for the answer.
        :param valid: Whether the answer is considered valid (e.g. sent before deadline, etc.)
        :param users: The id of the usergroup to which the answer belongs.
        :param task_id: The id of the task.
        :param content: The content of the answer.
        :param points: Points for the task.

        """
        if tags is None:
            tags = []
        existing_answers = self.get_common_answers(users, task_id)
        if len(existing_answers) > 0 and existing_answers[0].content == content:
            a = existing_answers[0]
            a.points = points
            a.last_points_modifier = points_given_by
            return None

        a = Answer(task_id=task_id, content=content, points=points, valid=valid, last_points_modifier=points_given_by)
        self.session.add(a)
        self.session.flush()
        answer_id = a.id
        assert answer_id > 0

        for u in users:
            ua = UserAnswer(user_id=u.id, answer_id=answer_id)
            self.session.add(ua)

        for tag in tags:
            at = AnswerTag(answer_id=answer_id, tag=tag)
            self.session.add(at)
        self.session.flush()
        return answer_id

    def get_all_answers(self,
                        task_ids: List[str],
                        usergroup: Optional[int],
                        hide_names: bool,
                        age: str,
                        valid: str,
                        printname: bool,
                        sort: str,
                        print_opt: str,
                        period_from: datetime,
                        period_to: datetime,
                        consent: Optional[Consent]) -> List[str]:
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
        print_header = print_opt == "all" or print_opt == "header"
        print_answers = print_opt == "all" or print_opt == "answers" or print_opt == "answersnoline"

        q = (Answer
             .query
             .filter((period_from <= Answer.answered_on) & (Answer.answered_on < period_to))
             .filter(Answer.task_id.in_(task_ids)))
        if valid == 'all':
            pass
        elif valid == '0':
            q = q.filter_by(valid=False)
        else:
            q = q.filter_by(valid=True)
        q = q.join(User, Answer.users)
        if consent is not None:
            q = q.filter_by(consent=consent)

        if age == "min":
            minmax = func.min(Answer.id).label('minmax')
            counts = func.count(Answer.answered_on).label('count')
        elif age == "all":
            minmax = Answer.id
            counts = Answer.valid
        else:
            minmax = func.max(Answer.id).label('minmax')
            counts = func.count(Answer.answered_on).label('count')

        q = q.add_columns(minmax, counts)
        if age != 'all':
            q = q.group_by(Answer.task_id, User.id)
        q = q.with_entities(minmax, counts)
        sub = q.subquery()
        q = Answer.query.join(sub, Answer.id == sub.c.minmax).join(User, Answer.users)
        if sort == 'username':
            q = q.order_by(User.name, Answer.task_id)
        else:
            q = q.order_by(Answer.task_id, User.name)
        q = q.with_entities(Answer, User, sub.c.count)
        result = []

        lf = "\n"
        if print_opt == "answersnoline":
            lf = ""

        qq: Iterable[Tuple[Answer, User, int]] = q
        cnt = 0
        for a, u, n in qq:
            cnt += 1
            points = str(a.points)
            if points == "None":
                points = ""
            name = u.name
            if hide_names:
                name = "user" + str(cnt)
            header = name + "; " + a.task_id + "; " + str(a.answered_on) + "; " + str(n) + "; " + points
            # print(separator + header)
            line = json.loads(a.content)
            answ = str(line)
            if isinstance(line, dict):  # maybe csPlugin?
                if "usercode" in line:  # is csPlugin
                    answ = line.get("usercode", "-")
                else:
                    if "points" in line:    # empty csPlugin answer
                        answ = ""

            res = ""
            if printname and not hide_names:
                header = str(u.real_name) + "; " + header
            if print_header:
                res = header
            if print_answers:
                res += lf + answ
            if print_opt == "korppi":
                res = name + ";"
                taskid = a.task_id
                i = taskid.find(".")
                if i >= 0:
                    taskid = taskid[i + 1:]
                res += taskid + ";" + answ.replace("\n", "\\n")

            result.append(res)
        return result

    def get_common_answers(self, users: List[User], task_id: str) -> List[Answer]:
        q = Answer.query.filter_by(task_id=task_id).join(User, Answer.users).filter(
            User.id.in_([u.id for u in users])).order_by(Answer.id.desc())

        def g():
            user_set = set(users)
            for a in q:  # type: Answer
                if not (user_set - set(a.users_all)):
                    yield a

        return list(g())

    def get_users_for_tasks(self, task_ids: List[str], user_ids: Optional[List[int]]=None, group_by_user=True, group_by_doc=False) -> List[Dict[str, str]]:
        if not task_ids:
            return []

        sub = r", substring(task_id from '(\d+)\..+')"
        subb = sub + " AS doc_id"

        if user_ids is None:
            user_ids = []
            user_restrict_sql = ''
        elif not user_ids:
            user_restrict_sql = 'AND FALSE'
        else:
            user_restrict_sql = f'AND UserAccount.id IN :user_ids'
        sql = f"""
                SELECT *, task_points + COALESCE(velp_points, 0) as total_points
                FROM (
                SELECT UserAccount.id, name, real_name, email,
                       COUNT(task_id) AS task_count{subb if group_by_doc else ''},
                       ROUND(SUM(cast(points as float))::numeric,4) as task_points,
                       ROUND(SUM(velp_points)::numeric,4) as velp_points,
                       COUNT(annotation_answer_id) AS velped_task_count
                       {', MIN(task_id) as task_id' if not group_by_user else ''}
                FROM UserAccount
                JOIN UserAnswer ON UserAccount.id = UserAnswer.user_id
                JOIN (

                      (SELECT Answer.task_id, UserAnswer.user_id as uid, MAX(Answer.id) as aid
                      FROM Answer
                      JOIN UserAnswer ON UserAnswer.answer_id = Answer.id
                      WHERE task_id IN :task_ids AND Answer.valid = TRUE
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
                {user_restrict_sql}
                GROUP BY UserAccount.id {'' if group_by_user else ', task_id'}{sub if group_by_doc else ''}
                ORDER BY real_name ASC
                ) tmp
            """
        result = tim_main_execute(sql, {'task_ids': tuple(task_ids), 'user_ids': tuple(user_ids)})
        return result_as_dict_list(result.cursor)

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
        result = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(list))))
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
                    group['task_sum'] = round(sum(t['task_points']
                                                  for t in group['tasks'] if t['task_points'] is not None), 2)
                if PointType.velp in rule.groups[groupname].point_types:
                    group['velp_sum'] = round(sum(t['velp_points']
                                                  for t in group['tasks'] if t['velp_points'] is not None), 2)
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
