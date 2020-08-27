""""""
import json
from collections import defaultdict, OrderedDict
from dataclasses import dataclass
from datetime import datetime
from typing import List, Optional, Dict, Tuple, Iterable, Any, Generator, TypeVar, SupportsRound, DefaultDict, Union, \
    TypedDict, MappingView, ItemsView

from bs4 import UnicodeDammit
from sqlalchemy import func, Numeric, Float, true
from sqlalchemy.dialects.postgresql import aggregate_order_by
from sqlalchemy.orm import selectinload, defaultload, Query

from timApp.answer.answer import Answer
from timApp.answer.answer_models import AnswerTag, UserAnswer
from timApp.answer.pointsumrule import PointSumRule, PointType, Group
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


def get_latest_valid_answers_query(task_id: TaskId, users: List[User]):
    sq = (Answer.query
        .filter_by(task_id=task_id.doc_task, valid=True)
        .join(User, Answer.users)
        .filter(User.id.in_([u.id for u in users]))
        .group_by(User.id)
        .with_entities(func.max(Answer.id).label('aid'), User.id.label('uid'))
        .subquery())
    datas = Answer.query.join(sq, Answer.id == sq.c.aid).with_entities(Answer)
    return datas


def is_redundant_answer(content: str, existing_answers: ExistingAnswersInfo, ptype: Optional[PluginType], valid: bool) -> bool:
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
        points_given_by: Optional[int]=None,
        force_save: bool = False,
        saver: Optional[User] = None,
        plugintype: Optional[PluginType] = None,
        max_content_len: Optional[int] = None,
) -> Optional[int]:
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
                    consent: Optional[Consent]) -> Union[List[str], List[Dict]]:
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
    result_json = []

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
        ns = str(int(n))  # n may be a boolean, so convert to int (0/1) first
        if hide_names:
            name = "user" + str(cnt)
        header = name + "; " + a.task_id + "; " + str(a.answered_on) + "; " + ns + "; " + points
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
            result_json.append(dict(user=u, answer=a, count=int(n), resolved_content=answ))
        else:
            raise RouteException(f'Unknown data format option: {data_format}')
    if data_format == 'text':
        return result
    else:
        return result_json


def get_all_answer_initial_query(
        period_from: datetime,
        period_to: datetime,
        task_ids: List[TaskId],
        valid: str,
) -> Query:
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


def valid_answers_query(task_ids: List[TaskId]) -> Query:
    return (Answer.query
            .filter(valid_taskid_filter(task_ids)))


def valid_taskid_filter(task_ids: List[TaskId]) -> Query:
    return Answer.task_id.in_(task_ids_to_strlist(task_ids)) & (Answer.valid == True)


class UserTaskEntry(TypedDict):
    user: User
    task_count: int
    velped_task_count: int
    total_points: Optional[float]
    task_points: Optional[float]
    velp_points: Optional[float]
    task_id: str


def get_users_for_tasks(
        task_ids: List[TaskId],
        user_ids: Optional[List[int]] = None,
        group_by_user: bool = True,
        group_by_doc: bool = False,
        answer_filter: Optional[Any] = None,
) -> List[UserTaskEntry]:
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
    velp_sum = func.round(func.sum(tmp.c.velp_points).cast(Numeric), 4).cast(Float).label(
        'velp_points')

    main = main.with_entities(
        User,
        func.count(tmp.c.task_id).label('task_count'),
        task_sum,
        velp_sum,
        # TODO: The following does not work because PostgreSQL evaluates a+b==null if a==null or b==null
        #  We want a+b to be null only if BOTH are null. For now, the summing is done in Python.
        # func.round((task_sum + velp_sum).cast(Numeric), 4).cast(Float).label('total_points'),
        func.count(tmp.c.annotation_answer_id).label('velped_task_count'),
        *cols,
    ).order_by(User.real_name)

    def g() -> Generator[UserTaskEntry, None, None]:
        for r in main:
            d = r._asdict()
            d['user'] = d.pop('User')
            task = d['task_points']
            velp = d['velp_points']
            if task is not None and velp is not None:
                tot = task + velp
            elif task is not None:
                tot = task
            else:
                tot = velp
            d['total_points'] = tot
            yield d

    result = list(g())
    return result


T = TypeVar('T', bound=SupportsRound[Any])


def sum_and_round(generator: Generator[T, None, None], digits: int = 2) -> Optional[T]:
    list_to_sum = list(generator)
    if not list_to_sum:
        return None
    return round(sum(list_to_sum), digits)  # type: ignore


def round_if_not_none(num: Optional[T], digits: int = 2) -> Optional[T]:
    if num is None:
        return None
    return round(num, digits)


class SumFields(TypedDict):
    task_sum: Optional[float]
    velp_sum: Optional[float]
    total_sum: Optional[float]


class UserPointGroup(SumFields):
    tasks: List[UserTaskEntry]
    velped_task_count: int


class UserPointInfo(SumFields):
    groups: DefaultDict[str, UserPointGroup]


class ResultGroup(SumFields):
    text: str
    link: bool
    linktext: str


class UserPoints(TypedDict):
    total_points: Optional[float]
    task_points: Optional[float]
    velp_points: Optional[float]
    task_count: int
    velped_task_count: int
    groups: Dict[str, ResultGroup]
    user: User


def get_points_by_rule(
        rule: Optional[PointSumRule],
        task_ids: List[TaskId],
        user_ids: Optional[List[int]] = None,
        answer_filter: Optional[Any] = None,
        force_user: Optional[User] = None,
) -> Union[List[UserPoints], List[UserTaskEntry]]:  # TODO: Would be better to return always same kind of list.
    """Computes the point sum from given tasks according to the given point rule.

    :param force_user: Whether to force at least one result user if the result would be empty otherwise.
    :param answer_filter: Optional additional filter for answers.
    :param rule: The points rule.
    :param task_ids: The list of task ids to consider.
    :param user_ids: The list of users for which to compute the sum.
    :return: The computed result.

    """
    if not rule:
        return get_users_for_tasks(task_ids, user_ids, answer_filter=answer_filter)
    tasks_users = get_users_for_tasks(task_ids, user_ids, group_by_user=False, answer_filter=answer_filter)
    result: DefaultDict[int, UserPointInfo] = (
        defaultdict(lambda: {
            'groups': defaultdict(lambda: {
                'tasks': [],
                'task_sum': None,
                'velp_sum': None,
                'total_sum': None,
                'velped_task_count': 0,
            }),
            'task_sum': None,
            'velp_sum': None,
            'total_sum': None,
        })
    )
    task_counts: Dict[int, int] = {}
    user_map = {}
    if not tasks_users and rule.force and force_user:
        for t in task_ids:
            tasks_users.append(UserTaskEntry(
                user=force_user,
                task_count=0,
                velped_task_count=0,
                velp_points=None,
                total_points=None,
                task_points=None,
                task_id=t.doc_task,
            ))
    for tu in tasks_users:
        u = tu['user']
        uid = u.id
        user_map[uid] = u
        if rule.count_all:  # TODO: would this info already been somewhere cheaper?
            c = task_counts.get(uid, 0)
            c += 1
            task_counts[uid] = c
        for grp in rule.find_groups(tu['task_id']):
            result[uid]['groups'][grp]['tasks'].append(tu)
    for user_id, task_groups in result.items():
        groups = task_groups['groups']
        groupsums = []
        for groupname, group in groups.items():
            task_sum = None
            velp_sum = None
            gr = rule.groups[groupname]
            if PointType.task in gr.point_types:
                task_sum = sum_and_round(t['task_points'] for t in group['tasks'] if t['task_points'] is not None)
            if PointType.velp in rule.groups[groupname].point_types:
                velp_sum = sum_and_round(t['velp_points'] for t in group['tasks'] if t['velp_points'] is not None)
            group['velped_task_count'] = sum(1 for t in group['tasks'] if t['velped_task_count'] > 0)
            if task_sum is not None and velp_sum is not None:
                total_sum: Optional[float] = task_sum + velp_sum
            elif task_sum is not None:
                total_sum = task_sum
            elif velp_sum is not None:
                total_sum = velp_sum
            else:
                total_sum = None
            total_sum = min(max(total_sum, gr.min_points), gr.max_points) if total_sum is not None else None
            group['task_sum'] = task_sum
            group['velp_sum'] = velp_sum
            group['total_sum'] = total_sum
            groupsums.append((task_sum, velp_sum, total_sum))
        groupsums = sorted(groupsums, reverse=rule.count_type == 'best', key=lambda x: (x[2] is not None, x[2]))
        if rule.total:
            s = groupsums[0]  # TODO: find indesies from total, total is a list of names to count
            task_groups['task_sum'] = round_if_not_none(s[0])
            task_groups['velp_sum'] = round_if_not_none(s[1])
            task_groups['total_sum'] = round_if_not_none(s[2])
        else:
            task_groups['task_sum'] = sum_and_round(s[0] for s in groupsums[0:rule.count_amount] if s[0] is not None)
            task_groups['velp_sum'] = sum_and_round(s[1] for s in groupsums[0:rule.count_amount] if s[1] is not None)
            task_groups['total_sum'] = sum_and_round(s[2] for s in groupsums[0:rule.count_amount] if s[2] is not None)
    return flatten_points_result(rule, result, task_counts, user_map)


def flatten_points_result(
        rule: PointSumRule,
        result: DefaultDict[int, UserPointInfo],
        task_counts: Dict[int, int],
        user_map: Dict[int, User],
) -> List[UserPoints]:
    result_list = []
    hide_list = rule.hide
    for user_id, task_groups in result.items():
        row = UserPoints(
            total_points=task_groups['total_sum'],
            task_points=task_groups['task_sum'],
            velp_points=task_groups['velp_sum'],
            task_count=task_counts.get(user_id, 0) if rule.count_all else sum(1 for t in task_groups['groups'].values() if sum(x['task_count'] for x in t['tasks']) > 0),
            velped_task_count=sum(1 for t in task_groups['groups'].values() if t['velped_task_count'] > 0),
            groups=OrderedDict(),
            user=user_map[user_id],
        )

        if rule.sort:
            rulegroups: Union[ItemsView[str, Group], List[Tuple[str, Group]]] = sorted(rule.groups.items())
        else:
            rulegroups = rule.groups.items()
        for groupname, rg in rulegroups:
            if hide_list and groupname in hide_list:
                continue
            gr = task_groups['groups'].get(groupname)
            if gr:
                task_sum = gr['task_sum']
                velp_sum = gr['velp_sum']
                total_sum = gr['total_sum']
            else:
                task_sum = None
                velp_sum = None
                total_sum = None
            try:
                linktext = rg.linktext or rule.linktext
                link = rg.link
            except:
                linktext = ""
                link = False
            try:
                text = rg.expl.format(
                    groupname,
                    float(total_sum if total_sum is not None else 0),
                    float(task_sum if task_sum is not None else 0),
                    float(velp_sum if velp_sum is not None else 0),
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


def add_missing_users_from_group(result: List, usergroup: UserGroup) -> List:
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
