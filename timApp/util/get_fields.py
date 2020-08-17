import itertools
import json
import re
import time
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from enum import Enum, unique
from typing import List, Optional, Tuple, DefaultDict, Dict, TypedDict, Any, Union

import attr
import dateutil.parser
from marshmallow import missing
from sqlalchemy import func, true
from sqlalchemy.orm import lazyload, joinedload
from werkzeug.exceptions import abort

from timApp.answer.answer import Answer
from timApp.answer.answers import get_points_by_rule, basic_tally_fields, valid_answers_query
from timApp.auth.accesshelper import get_doc_or_abort
from timApp.document.docinfo import DocInfo
from timApp.plugin.plugin import find_task_ids, CachedPluginFinder
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId
from timApp.user.groups import verify_group_view_access
from timApp.user.user import User, get_membership_end
from timApp.user.usergroup import UserGroup
from timApp.util.utils import widen_fields, get_alias, seq_to_str, fin_timezone

ALL_ANSWERED_WILDCARD = '*'

def chunks(l: List, n: int):
    for i in range(0, len(l), n):
        yield l[i:i + n]


tallyfield_re = re.compile(
    r'tally:((?P<doc>\d+)\.)?(?P<field>[a-zA-Z0-9öäåÖÄÅ_-]+)(\[ *(?P<ds>[^\[\],]*) *, *(?P<de>[^\[\],]*) *\])?'
)


@attr.s(auto_attribs=True)
class TallyField:
    """In Jsrunner, represents the "tally:" type of field."""
    field: str
    doc_id: Optional[int]
    datetime_start: Optional[datetime]
    datetime_end: Optional[datetime]
    default_doc: DocInfo

    @property
    def effective_doc_id(self):
        return self.doc_id or self.default_doc.id

    @property
    def grouping_key(self):
        return f'{self.effective_doc_id}{self.datetime_start}{self.datetime_end}'

    @property
    def doc_and_field(self):
        return f'{self.effective_doc_id}.{self.field}'

    @staticmethod
    def try_parse(s: str, default_doc: DocInfo) -> Optional['TallyField']:
        m = tallyfield_re.fullmatch(s)
        if not m:
            return None
        try:
            gds = m.group('ds')
            gde = m.group('de')
            ds, de = (dateutil.parser.parse(gds) if gds else None,
                      dateutil.parser.parse(gde) if gde else None)
        except (ValueError, OverflowError):
            return None
        if ds and ds.tzinfo is None:
            ds = fin_timezone.localize(ds)
        if de and de.tzinfo is None:
            de = fin_timezone.localize(de)
        doc = m.group('doc')
        return TallyField(
            field=m.group('field'),
            datetime_start=ds,
            datetime_end=de,
            doc_id=int(doc) if doc else None,
            default_doc=default_doc,
        )


@unique
class MembershipFilter(Enum):
    All = 'all'
    Current = 'current'
    Deleted = 'deleted'


member_filter_relation_map = {
    MembershipFilter.All: User.groups_dyn,
    MembershipFilter.Current: User.groups,
    MembershipFilter.Deleted: User.groups_inactive,
}

UserFields = Dict[str, Union[str, float, None]]


class UserFieldObj(TypedDict):
    user: User
    fields: UserFields
    styles: Any


@dataclass
class RequestedGroups:
    groups: List[UserGroup]
    include_all_answered:  bool = False


def get_fields_and_users(
        u_fields: List[str],
        requested_groups: RequestedGroups,
        d: DocInfo,
        current_user: User,
        autoalias: bool = False,
        add_missing_fields: bool = False,
        allow_non_teacher: bool = False,
        member_filter_type: MembershipFilter = MembershipFilter.Current,
        user_filter=None,
) -> Tuple[List[UserFieldObj], Dict[str, str], List[str], Optional[List[UserGroup]]]:
    """
    Return fielddata, aliases, field_names
    :param user_filter: Additional filter to use.
    :param member_filter_type: Whether to use all, current or deleted users in groups.
    :param u_fields: list of fields to be used
    :param requested_groups: requested user groups to be used; can contain None (means all answered users)
    :param d: default document
    :param current_user: current users, check his rights to fields
    :param autoalias: if true, give automatically from d1 same as would be from d1 = d1
    :param add_missing_fields: return estimated field even if it wasn't given previously
    :param allow_non_teacher: can be used also for non techers if othre rights matches
    :param user_groups: user groups to always include in the result. NOTE: this bypasses view access checks!
    :return: fielddata, aliases, field_names
    """
    if requested_groups.include_all_answered:
        allow_non_teacher = False
    needs_group_access_check = UserGroup.get_teachers_group() not in current_user.groups
    ugroups = []
    for group in requested_groups.groups:
        if not group:
            continue
        if needs_group_access_check and group.name != current_user.name:
            if not verify_group_view_access(group, current_user, require=False):
                # return abort(403, f'Missing view access for group {group.name}')
                continue  # TODO: study how to give just warning from missing access, extra return string?
        ugroups.append(group)

    if not ugroups:  # if no access, give at least own group
        ugroups.append(current_user.get_personal_group())
    groups = ugroups

    task_ids = []
    task_id_map = defaultdict(list)
    alias_map = {}
    jsrunner_alias_map = {}
    doc_map = {}

    try:
        u_fields = widen_fields(u_fields)
    except Exception as e:
        return abort(400, f"Problem with field names: {u_fields}\n{e}")

    tasks_without_fields = []
    tally_fields: List[Tuple[TallyField, Optional[str]]] = []
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
            task_id = TaskId.parse(
                t,
                require_doc_id=False,
                allow_block_hint=False,
                allow_type=False,
                allow_custom_field=True,
            )
        except PluginException as e:
            tally_field = TallyField.try_parse(t, d)
            if not tally_field:
                if t.startswith('tally:'):
                    return abort(400, f'Invalid tally field format: {t}')
                else:
                    return abort(400, str(e))
            else:
                did = tally_field.effective_doc_id
                tally_fields.append((tally_field, a))
                alias_map_value = tally_field.doc_and_field
        else:
            if task_id.field is None:
                tasks_without_fields.append(task_id)
            task_ids.append(task_id)

            if not task_id.doc_id:
                task_id.doc_id = d.id
            task_id_map[task_id.doc_task].append(task_id)
            did = task_id.doc_id
            alias_map_value = task_id.extended_or_doc_task
        if a:
            alias_map[alias_map_value] = a
            if a in jsrunner_alias_map:
                abort(400, f'Duplicate alias {a} in fields attribute')
            jsrunner_alias_map[a] = alias_map_value

        if did in doc_map:
            continue
        dib = get_doc_or_abort(did, f'Document {did} not found')
        if not (current_user.has_teacher_access(dib) or allow_non_teacher):
            abort(403, f'Missing teacher access for document {dib.id}')
        elif dib.document.get_settings().get('need_view_for_answers', False) \
                and not current_user.has_view_access(dib):
            abort(403, "Sorry, you don't have permission to use this resource.")
        doc_map[did] = dib

    cpf = CachedPluginFinder(doc_map=doc_map, curr_user=current_user)
    if add_missing_fields:
        for task in tasks_without_fields:
            plug = cpf.find(task)
            if plug:
                task.field = plug.get_content_field_name()
            else:
                task.field = "c"
            try:
                alias_map[task.doc_task_with_field] = alias_map[task.doc_task]
                jsrunner_alias_map[alias_map[task.doc_task]] = task.doc_task_with_field
                del alias_map[task.doc_task]
            except KeyError:
                pass

    group_id_set = set(ug.id for ug in groups)
    group_filter = UserGroup.id.in_([ug.id for ug in groups])
    if user_filter is not None:
        group_filter = group_filter & user_filter
    join_relation = member_filter_relation_map[member_filter_type]
    tally_field_values = get_tally_field_values(d,
                                                doc_map,
                                                group_filter if not requested_groups.include_all_answered else None,
                                                join_relation,
                                                tally_fields)
    sub = []
    # For some reason, with 7 or more fields, executing the following query is very slow in PostgreSQL 9.5.
    # That's why we split the list of task ids in chunks of size 6 and merge the results.
    # TODO: Investigate if this is still true for PostgreSQL 11.
    not_global_taskids = [t for t in task_ids if not t.is_global]
    for task_chunk in chunks(not_global_taskids, 6):
        q = valid_answers_query(task_chunk).join(User, Answer.users)
        if not requested_groups.include_all_answered:
            q = q.join(UserGroup, join_relation).filter(group_filter)
        sub += (
                q
                .group_by(Answer.task_id, User.id)
                .with_entities(func.max(Answer.id), User.id)
                .all()
        )
    aid_uid_map = defaultdict(list)
    user_ids = set()
    for aid, uid in sub:
        aid_uid_map[aid].append(uid)
        user_ids.add(uid)

    q1 = User.query.join(UserGroup, join_relation).filter(group_filter)
    if requested_groups.include_all_answered:
        # if no group filter is given, attempt to get users that have valid answers only using the user
        # ids from previous query
        id_filter = User.id.in_(user_ids)
        # Ensure that user filter gets applied even if group filter was None
        if user_filter is not None:
            id_filter = id_filter & user_filter
        q2 = User.query.filter(id_filter)
        q = q1.union(q2)
    else:
        q = q1
    q = q.with_entities(User).order_by(User.id).options(lazyload(User.groups))
    if member_filter_type != MembershipFilter.Current:
        q = q.options(joinedload(User.memberships))
    users: List[User] = q.all()
    user_map = {}
    for u in users:
        user_map[u.id] = u
    global_taskids = [t for t in task_ids if t.is_global]
    global_answer_ids = valid_answers_query(global_taskids).group_by(Answer.task_id).with_entities(
        func.max(Answer.id)).all()
    answs = Answer.query.filter(Answer.id.in_(itertools.chain((aid for aid, _ in sub), global_answer_ids))).all()
    answers_with_users: List[Tuple[int, Optional[Answer]]] = []
    for a in answs:
        uids = aid_uid_map.get(a.id)
        if uids is not None:
            for uid in uids:
                answers_with_users.append((uid, a))
        else:
            # This is a global task, so add the answer for all users.
            for uid in user_map.keys():
                answers_with_users.append((uid, a))
    missing_users = set(u.id for u in users) - set(uid for uid, _ in answers_with_users)
    for mu in missing_users:
        answers_with_users.append((mu, None))
    answers_with_users.sort(key=lambda au: au[0])
    last_user = None
    user_tasks = None
    user_fieldstyles = None
    user_index = -1
    res: List[UserFieldObj] = []
    for uid, a in answers_with_users:
        if last_user != uid:
            user_index += 1
            tally_values = tally_field_values.get(uid)
            if tally_values:
                user_tasks = {a: v for v, a in tally_values}
            else:
                user_tasks = {}
            user_fieldstyles = {}
            user = users[user_index]
            obj = {'user': user, 'fields': user_tasks, 'styles': user_fieldstyles}
            res.append(obj)
            if member_filter_type != MembershipFilter.Current:
                m_end = get_membership_end(user, group_id_set)
                if m_end:
                    obj['groupinfo'] = {'membership_end': time.mktime(m_end.timetuple())}
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
                elif task.field == "ALL":
                    value = p
                else:
                    if task.field:
                        try:
                            value = p.get(task.field)
                        except:
                            value = json.dumps(p)
                    else:
                        if len(json_str) > 1:
                            plug = cpf.find(task)
                            if plug:
                                content_field = plug.get_content_field_name()
                            else:
                                content_field = "c"
                            if isinstance(p, dict):
                                value = p.get(content_field)
                            else:
                                value = p
                        else:
                            values_p = list(p.values())
                            value = values_p[0]
            user_tasks[alias_map.get(task.extended_or_doc_task, task.extended_or_doc_task)] = value
            user_fieldstyles[alias_map.get(task.extended_or_doc_task, task.extended_or_doc_task)] = style
    return (
        res,
        jsrunner_alias_map,
        [alias_map.get(ts.extended_or_doc_task, ts.extended_or_doc_task) for ts in task_ids],
        groups,
    )


def get_tally_field_values(
        d: DocInfo,
        doc_map: Dict[int, DocInfo],
        group_filter,
        join_relation,
        tally_fields: List[Tuple[TallyField, Optional[str]]],
):
    tally_field_values: DefaultDict[int, List[Tuple[float, str]]] = defaultdict(list)
    task_id_cache = {}
    field_groups = itertools.groupby(tally_fields, key=lambda f: f[0].grouping_key)
    for _, x in field_groups:
        fs = list(x)
        g = fs[0][0]
        doc = doc_map[g.doc_id].document if g.doc_id else d.document
        tids = task_id_cache.get(doc.doc_id)
        if tids is None:
            doc.insert_preamble_pars()
            pars = doc.get_dereferenced_paragraphs()
            tids = find_task_ids(pars, check_access=False)[0]
            task_id_cache[doc.doc_id] = tids
        ans_filter = true()
        if g.datetime_start:
            ans_filter = ans_filter & (Answer.answered_on >= g.datetime_start)
        if g.datetime_end:
            ans_filter = ans_filter & (Answer.answered_on < g.datetime_end)
        psr = doc.get_settings().point_sum_rule()
        pts = get_points_by_rule(
            points_rule=psr,
            task_ids=tids,
            user_ids=User.query.join(UserGroup, join_relation)
                .filter(group_filter)
                .with_entities(User.id)
                .subquery() if group_filter is not None else None,
            flatten=True,
            answer_filter=ans_filter,
        )

        known_tally_fields = list(itertools.chain(basic_tally_fields, psr.groups if psr else []))
        for field, _ in fs:
            if field.field not in known_tally_fields:
                abort(400, f'Unknown tally field: {field.field}. '
                           f'Valid tally fields are: {seq_to_str(known_tally_fields)}.')
        for r in pts:
            u: User = r.pop('user')
            groups = r.pop('groups', None)
            for field, alias in fs:
                # The value can be None if the user has not done any tasks with points, so we use another sentinel.
                value = r.get(field.field, missing)
                if value is missing:
                    value = groups[field.field]  # The group should exist because the field was validated above.
                    value = value['total_sum']
                tally_field_values[u.id].append((value, alias or field.doc_and_field))
    return tally_field_values
