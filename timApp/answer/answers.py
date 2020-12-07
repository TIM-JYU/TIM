""""""
import json
from collections import defaultdict, OrderedDict
from dataclasses import dataclass
from datetime import datetime
from operator import itemgetter
from typing import List, Optional, Dict, Tuple, Iterable, Any

from bs4 import UnicodeDammit
from sqlalchemy import func, Numeric, Float, true
from sqlalchemy.dialects.postgresql import aggregate_order_by
from sqlalchemy.orm import selectinload, defaultload, Query

from timApp.answer.answer import Answer
from timApp.answer.answer_models import AnswerTag, UserAnswer
from timApp.answer.pointsumrule import PointSumRule, PointType
from timApp.plugin.plugintype import PluginType
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.upload.upload import get_pluginupload
from timApp.user.user import Consent, User
from timApp.user.usergroup import UserGroup
from timApp.util.answerutil import task_ids_to_strlist
from timApp.util.flask.requesthelper import RouteException
from timApp.util.logger import log_warning
from timApp.velp.annotation_model import Annotation


@dataclass
class ExistingAnswersInfo:
    latest_answer: Optional[Answer]
    count: int


def get_latest_answers_query(task_id: TaskId, users: List[User]) -> Query:
    if task_id.is_global:
        q = Answer.query.filter_by(task_id=task_id.doc_task).order_by(Answer.id.desc())
    else:
        q = (
            Answer.query
                .filter_by(task_id=task_id.doc_task)
                .join(User, Answer.users)
                .filter(User.id.in_([u.id for u in users]))
                .group_by(Answer.id)
                .with_entities(Answer.id)
                .having((func.array_agg(aggregate_order_by(User.id, User.id))) == sorted([u.id for u in users]))
                .subquery()
        )
        q = Answer.query.filter(Answer.id.in_(q)).order_by(Answer.id.desc())
    return q


def is_redundant_answer(content: str, existing_answers: ExistingAnswersInfo, ptype: Optional[PluginType], valid: bool):
    la = existing_answers.latest_answer
    is_redundant = la and (la.content == content and la.valid == valid)
    if is_redundant:
        return True
    if existing_answers.count == 0 and ptype and ptype.type == 'rbfield' and json.loads(content)['c'] == '0':
        return True
    return False


class TooLargeAnswerException(Exception):
    pass


def save_answer(
        users: List[User],
        task_id: TaskId,
        content: Any,
        points: Optional[float],
        tags: Optional[List[str]] = None,
        valid: bool = True,
        points_given_by=None,
        force_save=False,
        saver: User = None,
        plugintype: Optional[PluginType] = None,
        max_content_len: Optional[int] = None,
):
    """Saves an answer to the database.

    :param max_content_len: Maximum length for answer content.
    :param plugintype: The plugin type.
    :param saver: Who saved the answer.
    :param points_given_by: The usergroup id who gave the points, or None if they were given by a plugin.
    :param tags: Tags for the answer.
    :param valid: Whether the answer is considered valid (e.g. sent before deadline, etc.)
    :param users: The users to which the answer belongs.
    :param task_id: The id of the task.
    :param content: The content of the answer.
    :param points: Points for the task.
    :param force_save: Whether to force to save the answer even if the latest existing answer has the same content.

    """
    content_str = json.dumps(content)
    content_len = len(content_str)
    if max_content_len and content_len > max_content_len:
        log_warning(f'Task {task_id.doc_task}: too large answer ({content_len})')
        raise TooLargeAnswerException(f'Answer is too large (size is {content_len} but maximum is {max_content_len}).')
    if tags is None:
        tags = []
    answerinfo = get_existing_answers_info(users, task_id)
    if is_redundant_answer(content_str, answerinfo, plugintype, valid) and not force_save:
        if answerinfo.latest_answer:
            a = answerinfo.latest_answer
            a.points = points
            a.last_points_modifier = points_given_by
        return None

    a = Answer(task_id=task_id.doc_task, content=content_str, points=points, valid=valid,
               last_points_modifier=points_given_by)
    db.session.add(a)

    for u in users:
        a.users.append(u)

    for tag in tags:
        at = AnswerTag(tag=tag)
        a.tags.append(at)
    if saver:
        a.saver = saver
    db.session.flush()
    return a.id


def get_all_answers(task_ids: List[TaskId],
                    usergroup: Optional[int],
                    hide_names: bool,
                    age: str,
                    valid: str,
                    printname: bool,
                    sort: str,
                    print_opt: str,
                    period_from: datetime,
                    period_to: datetime,
                    data_format: str,
                    consent: Optional[Consent]) -> List[str]:
    """Gets all answers to the specified tasks.

    :param data_format: The data format to use, currently supports "text" and "json".
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

    q = get_all_answer_initial_query(period_from, period_to, task_ids, valid)

    q = q.options(defaultload(Answer.users).lazyload(User.groups))
    if consent is not None:
        q = q.filter_by(consent=consent)

    if age == "min":
        minmax = func.min(Answer.id).label('minmax')
        counts = func.count(Answer.answered_on).label('count')
    elif age == "all":
        minmax = Answer.id.label('minmax')
        counts = Answer.valid.label('count')
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
        q = q.order_by(User.name, Answer.task_id, Answer.answered_on)
    else:
        q = q.order_by(Answer.task_id, User.name, Answer.answered_on)
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
        n = str(int(n))  # n may be a boolean, so convert to int (0/1) first
        if hide_names:
            name = "user" + str(cnt)
        header = name + "; " + a.task_id + "; " + str(a.answered_on) + "; " + n + "; " + points
        line = json.loads(a.content)
        answ = json.dumps(line, ensure_ascii=False)
        if isinstance(line, dict):  # maybe csPlugin?
            files = line.get('uploadedFiles')
            if isinstance(files, list):
                if len(files) == 1:
                    p = files[0]['path']
                    prefix = '/uploads/'
                    if p.startswith(prefix):
                        p = p[len(prefix):]
                    mt, pu = get_pluginupload(p)
                    if mt == 'text/plain':
                        try:
                            answ = pu.data.decode()
                        except UnicodeDecodeError:
                            answ = UnicodeDammit(pu.data).unicode_markup
                    else:
                        answ = 'ERROR: Uploaded file is binary; cannot show content.'
                else:
                    answ = f'ERROR: There are more than 1 file uploads ({len(files)}) in this answer; cannot show content.'
            elif "usercode" in line:
                answ = str(line.get("usercode", "-"))
            else:
                if "points" in line:  # empty csPlugin answer
                    answ = ""

        if data_format == 'text':
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
        elif data_format == 'json':
            result.append(dict(user=u, answer=a, count=int(n), resolved_content=answ))
        else:
            raise RouteException(f'Unknown data format option: {data_format}')
    return result


def get_all_answer_initial_query(period_from, period_to, task_ids, valid) -> Query:
    q = (Answer
         .query
         .filter((period_from <= Answer.answered_on) & (Answer.answered_on < period_to))
         .filter(Answer.task_id.in_(task_ids_to_strlist(task_ids))))
    if valid == 'all':
        pass
    elif valid == '0':
        q = q.filter_by(valid=False)
    else:
        q = q.filter_by(valid=True)
    q = q.join(User, Answer.users)
    return q


def get_existing_answers_info(users: List[User], task_id: TaskId) -> ExistingAnswersInfo:
    q = get_latest_answers_query(task_id, users)
    latest = q.first()
    count = q.count()
    return ExistingAnswersInfo(latest_answer=latest, count=count)


basic_tally_fields = [
    'total_points',
    'velp_points',
    'task_points',
    'task_count',
    'velped_task_count',
]


def valid_answers_query(task_ids: List[TaskId]):
    return (Answer.query
            .filter(valid_taskid_filter(task_ids)))


def valid_taskid_filter(task_ids: List[TaskId]):
    return Answer.task_id.in_(task_ids_to_strlist(task_ids)) & (Answer.valid == True)


def get_users_for_tasks(task_ids: List[TaskId], user_ids: Optional[List[int]] = None, group_by_user=True,
                        group_by_doc=False,
                        answer_filter=None) -> List[Dict[str, Any]]:
    if not task_ids:
        return []

    a3 = (Annotation.query
          .filter_by(valid_until=None)
          .group_by(Annotation.answer_id)
          .with_entities(Annotation.answer_id.label('annotation_answer_id'),
                         func.sum(Annotation.points).label('velp_points'))
          .subquery())
    a2 = Answer.query.with_entities(Answer.id, Answer.points).subquery()
    if answer_filter is None:
        answer_filter = true()
    a1 = (valid_answers_query(task_ids)
          .filter(answer_filter)
          .join(UserAnswer, UserAnswer.answer_id == Answer.id)
          .group_by(UserAnswer.user_id, Answer.task_id)
          .with_entities(Answer.task_id,
                         UserAnswer.user_id.label('uid'),
                         func.max(Answer.id).label('aid')).subquery())
    tmp = (db.session.query(a1, a2, a3)
           .join(a2, a1.c.aid == a2.c.id)
           .outerjoin(a3, a3.c.annotation_answer_id == a1.c.aid)
           .subquery())
    main = (User.query
            .join(UserAnswer, UserAnswer.user_id == User.id)
            .join(tmp, (tmp.c.aid == UserAnswer.answer_id) & (User.id == tmp.c.uid)))
    group_by_cols = []
    cols = []
    if not group_by_user:
        min_task_id = func.min(tmp.c.task_id).label('task_id')
        group_by_cols.append(tmp.c.task_id)
        cols.append(min_task_id)
    if group_by_doc:
        doc_id = func.substring(tmp.c.task_id, '(\d+)\..+').label('doc_id')
        group_by_cols.append(doc_id)
        cols.append(doc_id)
    if user_ids is not None:
        main = main.filter(User.id.in_(user_ids))
    main = main.group_by(User.id, *group_by_cols)

    # prevents error:
    # column "usergroup_1.id" must appear in the GROUP BY clause or be used in an aggregate function
    main = main.options(selectinload(User.groups))

    task_sum = func.round(func.sum(tmp.c.points).cast(Numeric), 4).cast(Float).label('task_points')
    velp_sum = func.round(func.coalesce(func.sum(tmp.c.velp_points).cast(Numeric), 0), 4).cast(Float).label(
        'velp_points')

    main = main.with_entities(
        User,
        func.count(tmp.c.task_id).label('task_count'),
        task_sum,
        velp_sum,
        func.round((task_sum + velp_sum).cast(Numeric), 4).cast(Float).label('total_points'),
        func.count(tmp.c.annotation_answer_id).label('velped_task_count'),
        *cols,
    ).order_by(User.real_name)

    def g():
        for r in main:
            d = r._asdict()
            d['user'] = d.pop('User')
            yield d

    result = list(g())
    return result


def get_points_by_rule(points_rule: Optional[PointSumRule],
                       task_ids: List[TaskId],
                       user_ids: Optional[List[int]] = None,
                       flatten: bool = False,
                       answer_filter=None,
                       ):
    """Computes the point sum from given tasks accoring to the given point rule.

    :param answer_filter: Optional additional filter for answers.
    :param points_rule: The points rule.
    :param task_ids: The list of task ids to consider.
    :param user_ids: The list of users for which to compute the sum.
    :param flatten: Whether to return the result as a list of dicts (True) or as a deep dict (False).
    :return: The computed result.

    """
    if not points_rule:
        return get_users_for_tasks(task_ids, user_ids, answer_filter=answer_filter)
    tasks_users = get_users_for_tasks(task_ids, user_ids, group_by_user=False, answer_filter=answer_filter)
    rule = points_rule
    result = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(list))))
    task_counts = {}
    for tu in tasks_users:
        uid = tu['user'].id
        if points_rule.count_all:  # TODO: would this info allready been somewhere cheaper?
            c = task_counts.get(uid, 0)
            c += 1
            task_counts[uid] = c
        for group in rule.find_groups(tu['task_id']):
            result[uid]['groups'][group]['tasks'].append(tu)
    for user_id, task_groups in result.items():
        groups = task_groups['groups']
        groupsums = []
        for groupname, group in groups.items():
            group['task_sum'] = 0
            group['velp_sum'] = 0
            gr = rule.groups[groupname]
            if PointType.task in gr.point_types:
                group['task_sum'] = round(sum(t['task_points']
                                              for t in group['tasks'] if t['task_points'] is not None), 2)
            if PointType.velp in rule.groups[groupname].point_types:
                group['velp_sum'] = round(sum(t['velp_points']
                                              for t in group['tasks'] if t['velp_points'] is not None), 2)
            group['velped_task_count'] = sum(1 for t in group['tasks'] if t['velped_task_count'] > 0)
            total_sum = group['task_sum'] + group['velp_sum']
            group['total_sum'] = min(max(total_sum, gr.min_points), gr.max_points)
            groupsums.append((group['task_sum'], group['velp_sum'], group['total_sum']))
        if rule.count_type == 'best':
            groupsums = sorted(groupsums, reverse=True, key=itemgetter(2))
        else:
            groupsums = sorted(groupsums, key=itemgetter(2))
        try:
            if points_rule.total:
                s = groupsums[0]  # TODO: find indesies from total, total is a list of names to count
                task_groups['task_sum'] = round(s[0], 2)
                task_groups['velp_sum'] = round(s[1], 2)
                task_groups['total_sum'] = round(s[2], 2)
            else:
                task_groups['task_sum'] = round(sum(s[0] for s in groupsums[0:rule.count_amount]), 2)
                task_groups['velp_sum'] = round(sum(s[1] for s in groupsums[0:rule.count_amount]), 2)
                task_groups['total_sum'] = round(sum(s[2] for s in groupsums[0:rule.count_amount]), 2)
        except TypeError:
            task_groups['task_sum'] = 0
            task_groups['velp_sum'] = 0
            task_groups['total_sum'] = 0
    if flatten:
        result_list = []
        hide_list = points_rule.hide
        if (points_rule.force and not result.items()): # fake result
            fake_groups = {}
            fake_groups['task_sum'] = 0
            fake_groups['velp_sum'] = 0
            fake_groups['total_sum'] = 0
            fake_groups['groups'] = {}
            result[0] = fake_groups
        for user_id, task_groups in result.items():
            first_group = {'tasks': [{}]}
            try:  # TODO: tämä koska en osannut täyttää fake_groups oikein
                first_group = next(v for _, v in task_groups['groups'].items())
            except:
                pass
            row = first_group['tasks'][0]
            row['total_points'] = task_groups['total_sum']
            row['task_points'] = task_groups['task_sum']
            row['velp_points'] = task_groups['velp_sum']
            row['breaklines'] = points_rule.breaklines
            row['force'] = points_rule.force
            if points_rule.count_all: # note:  tasks_done = info[0]['task_count']
                row['task_count'] = task_counts.get(user_id, 0)
            else:
                row['task_count'] = len(task_groups['groups'])
            row['velped_task_count'] = sum(1 for t in task_groups['groups'].values() if t['velped_task_count'] > 0)
            row.pop('task_id', None)
            row['groups'] = OrderedDict()
            rulegroups = rule.groups.items()
            if points_rule.sort:
                rulegroups =  sorted(rulegroups)
            for groupname, rg in rulegroups:
                    if hide_list and groupname in hide_list:
                        continue
                    gr = task_groups['groups'].get(groupname, {})
                    expl = rg.expl
                    task_sum = gr.get('task_sum', 0)
                    velp_sum = gr.get('velp_sum', 0)
                    total_sum = gr.get('total_sum', 0)
                    try:
                        linktext = rg.linktext or rule.linktext
                        link = rg.link
                    except:
                        linktext = ""
                        link = False
                    try:
                        text = expl.format(groupname,
                                            float(total_sum),
                                            float(task_sum),
                                            float(velp_sum)
                                           )
                    except:
                        text = groupname + ": " + str(total_sum)
                    row['groups'][groupname] = {
                        'task_sum': task_sum,
                        'velp_sum': velp_sum,
                        'total_sum': total_sum,
                        'text': text,
                        'link': link,
                        'linktext': linktext
                    }
            result_list.append(row)
        return result_list
    return result


def add_missing_users_from_group(result: List, usergroup: UserGroup):
    users = set(usergroup.users)
    existing_users = set()
    for d in result:
        existing_users.add(d['user'])

    missing = users - existing_users

    for d in missing:
        result.append({'task_count': 0,
                       'task_points': None,
                       'velp_points': 0.0,
                       'total_points': None,
                       'velped_task_count': 0,
                       'user': d,
                       'id': d.id,
                       'name': d.name,
                       'real_name': d.real_name,
                       'email': d.email})

    # {'task_count': 1, 'task_points': None, 'velp_points': 0.0, 'total_points': None, 'velped_task_count': 0,
    # 'user': <User 6>, 'id': 6, 'name': 'testiuser@testi.fi', 'real_name': 'Testi User', 'email': 'testiuser@testi.fi'}

    return result
