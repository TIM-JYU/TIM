"""Answer-related routes."""
import json
import time
from collections import defaultdict
from datetime import timezone, timedelta, datetime
from typing import Union, List

import dateutil.parser
import dateutil.relativedelta
from flask import abort
from flask import request
from sqlalchemy import func
from sqlalchemy.orm import defaultload

from timApp.answer.answer import Answer
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docinfo import DocInfo
from timApp.plugin.plugin import TaskNotFoundException
from timApp.plugin.plugin import find_plugin_from_document
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_view_access
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import get_option
from timApp.util.utils import get_current_time, widen_fields, get_alias


def points_to_float(points: Union[str, float]):
    if points:
        points = float(points)
    else:
        points = None
    return points


def chunks(l: List, n: int):
    for i in range(0, len(l), n):
        yield l[i:i + n]


def task_ids_to_strlist(ids: List[TaskId]):
    return [t.doc_task for t in ids]


def get_fields_and_users(u_fields: List[str], groups: List[UserGroup],
                         d: DocInfo, current_user: User, autoalias: bool = False,
                         add_missing_fields: bool = False, allow_non_teacher: bool = False, valid_only: bool = True):
    """
    Return fielddata, aliases, field_names
    :param u_fields: list of fields to be used
    :param groups: user groups to be used
    :param d: default document
    :param current_user: current users, check his rights to fields
    :param autoalias: if true, give automatically from d1 same as would be from d1 = d1
    :param add_missing_fields: return estimated field even if it wasn't given previously
    :param allow_non_teacher: can be used also for non techers if othre rights matches
    :return: fielddata, aliases, field_names
    """
    needs_group_access_check = UserGroup.get_teachers_group() not in current_user.groups
    ugroups = []
    for group in groups:
        if needs_group_access_check and group.name != current_user.name:
            if not verify_group_view_access(group, current_user, require=False):
                # return abort(403, f'Missing view access for group {group.name}')
                continue  # TODO: study how to give just warning from missing access, extra return string?
        ugroups.append(group)

    if not ugroups:  # if no access, give at least own group
        for group in current_user.groups:
            if group.name == current_user.name:
                # print(group.name + " added just user group")
                ugroups.append(group)

    groups = ugroups

    task_ids = []
    task_id_map = defaultdict(list)
    alias_map = {}
    jsrunner_alias_map = {}
    doc_map = {}
    # num_prog = re.compile('^\d+\..+/')  # moved to widen_fields

    try:
        u_fields = widen_fields(u_fields)
    except Exception as e:
        return abort(400, f"Problem with field names: {u_fields}\n" + str(e))

    tasks_without_fields = []
    for field in u_fields:
        try:
            t, a, *rest = field.split("=")
        except ValueError:
            a = None
            t, rest = field, None
            if autoalias:
                a = get_alias(t)
        t = t.strip()
        if a:
            a = a.strip()
        if rest:
            return abort(400, f'Invalid alias: {field}')
        if a == '':
            return abort(400, f'Alias cannot be empty: {field}')
        try:
            task_id = TaskId.parse(t, False, False, add_missing_fields)
        except PluginException as e:
            return abort(400, str(e))
        if task_id.field is None:
            tasks_without_fields.append(task_id)
        task_ids.append(task_id)
        if not task_id.doc_id:
            task_id.doc_id = d.id
        task_id_map[task_id.doc_task].append(task_id)
        if a:
            alias = a
            if alias in jsrunner_alias_map:
                abort(400, f'Duplicate alias {alias} in fields attribute')
            alias_map[task_id.extended_or_doc_task] = alias
            jsrunner_alias_map[alias] = task_id.extended_or_doc_task
        if task_id.doc_id in doc_map:
            continue
        dib = get_doc_or_abort(task_id.doc_id, f'Document {task_id.doc_id} not found')

        if not (current_user.has_teacher_access(dib) or allow_non_teacher):
            abort(403, f'Missing teacher access for document {dib.id}')
        doc_map[task_id.doc_id] = dib.document

    if add_missing_fields:
        for task in tasks_without_fields:
            try:
                plug = find_plugin_from_document(doc_map[task.doc_id], task, current_user)
                task.field = plug.get_content_field_name()
            except TaskNotFoundException:
                task.field = "c"
            try:
                alias_map[task.doc_task_with_field] = alias_map[task.doc_task]
                jsrunner_alias_map[alias_map[task.doc_task]] = task.doc_task_with_field
                del alias_map[task.doc_task]
            except KeyError:
                pass

    res = []
    group_filter = UserGroup.id.in_([ug.id for ug in groups])
    sub = []

    # For some reason, with 7 or more fields, executing the following query is very slow.
    # That's why we split the list of task ids in chunks of size 6 and merge the results.
    if valid_only:
        filt = (Answer.valid == True )
    else:
        filt = True
    for task_chunk in chunks(task_ids, 6):
        sub += (
            Answer.query.filter(Answer.task_id.in_(task_ids_to_strlist(task_chunk)) & filt )
                .join(User, Answer.users)
                .join(UserGroup, User.groups)
                .filter(group_filter)
                .group_by(Answer.task_id, User.id)
                .with_entities(func.max(Answer.id), User.id)
                .all()
        )
    aid_uid_map = {}
    for aid, uid in sub:
        aid_uid_map[aid] = uid
    users = (
        UserGroup.query.filter(group_filter)
            .join(User, UserGroup.users)
            .options(defaultload(UserGroup.users).lazyload(User.groups))
            .with_entities(User)
            .order_by(User.id)
            .all()
    )
    user_map = {}
    for u in users:
        user_map[u.id] = u
    answs = Answer.query.filter(Answer.id.in_(aid for aid, _ in sub)).all()
    answers_with_users = []
    for a in answs:
        answers_with_users.append((aid_uid_map[a.id], a))
    missing_users = set(u.id for u in users) - set(uid for uid, _ in answers_with_users)
    for mu in missing_users:
        answers_with_users.append((mu, None))
    answers_with_users.sort(key=lambda x: x[0])
    last_user = None
    user_tasks = None
    user_fieldstyles = None
    user_index = -1
    user = None
    # style_map = {}
    for uid, a in answers_with_users:
        if last_user != uid:
            user_index += 1
            user_tasks = {}
            user_fieldstyles = {}
            user = users[user_index]
            res.append({'user': user, 'fields': user_tasks, 'styles': user_fieldstyles})
            last_user = uid
            if not a:
                continue
        for task in task_id_map[a.task_id]:
            value = None
            style = None
            if a:
                json_str = a.content
                p = json.loads(json_str)
                if isinstance(p, dict):
                    style = p.get('styles')
                if task.field == "points":
                    value = a.points
                elif task.field == "datetime":
                    value = time.mktime(a.answered_on.timetuple())
                else:

                    if task.field:

                        value = p.get(task.field)
                    else:
                        if len(p) > 1:
                            try:
                                plug = find_plugin_from_document(doc_map[task.doc_id], task, user)
                                content_field = plug.get_content_field_name()
                            except TaskNotFoundException:
                                content_field = "c"
                            value = p.get(content_field)
                        else:
                            values_p = list(p.values())
                            value = values_p[0]
            user_tasks[alias_map.get(task.extended_or_doc_task, task.extended_or_doc_task)] = value
            user_fieldstyles[alias_map.get(task.extended_or_doc_task, task.extended_or_doc_task)] = style
    return res, jsrunner_alias_map, [alias_map.get(ts.extended_or_doc_task, ts.extended_or_doc_task) for ts in task_ids]


def get_hidden_name(user_id):
    return 'Student %d' % user_id


def should_hide_name(d: DocInfo, user: User):
    return True
    # return not user.has_teacher_access(d) and user.id != get_current_user_id()


def maybe_hide_name(d: DocInfo, u: User):
    if should_hide_name(d, u):
        u.hide_name = True


def period_handling(task_ids, doc_ids, period):
    """
    Returns start and end of an period for answer results.
    :param task_ids: Task ids containing the answers.
    :param doc_ids: Documents containing the answers.
    :param period: Period options: whenever, sincelast, day, week, month, other.
    :return: Return "from"-period and "to"-period.
    """
    period_from = datetime.min.replace(tzinfo=timezone.utc)
    period_to = get_current_time()

    since_last_key = task_ids[0].doc_task if task_ids else None
    if len(task_ids) > 1:
        since_last_key = str(next(d for d in doc_ids))
        if len(doc_ids) > 1:
            since_last_key = None

        # Period from which to take results.
    if period == 'whenever':
        pass
    elif period == 'sincelast':
        u = get_current_user_object()
        prefs = u.get_prefs()
        last_answer_fetch = prefs.last_answer_fetch
        period_from = last_answer_fetch.get(since_last_key, datetime.min.replace(tzinfo=timezone.utc))
        last_answer_fetch[since_last_key] = get_current_time()
        prefs.last_answer_fetch = last_answer_fetch
        u.set_prefs(prefs)
        db.session.commit()
    elif period == 'day':
        period_from = period_to - timedelta(days=1)
    elif period == 'week':
        period_from = period_to - timedelta(weeks=1)
    elif period == 'month':
        period_from = period_to - dateutil.relativedelta.relativedelta(months=1)
    elif period == 'other':
        period_from_str = get_option(request, 'periodFrom', period_from.isoformat())
        period_to_str = get_option(request, 'periodTo', period_to.isoformat())
        try:
            period_from = dateutil.parser.parse(period_from_str)
        except (ValueError, OverflowError):
            pass
        try:
            period_to = dateutil.parser.parse(period_to_str)
        except (ValueError, OverflowError):
            pass

    return period_from, period_to
