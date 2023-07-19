import itertools
import json
import re
import time
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from enum import Enum, unique
from typing import Optional, DefaultDict, TypedDict, Any, Union

import attr
import dateutil.parser
from isodate import datetime_isoformat
from marshmallow import missing
from sqlalchemy import func, true, select
from sqlalchemy.orm import lazyload, selectinload

from timApp.answer.answer import Answer
from timApp.answer.answers import (
    get_points_by_rule,
    basic_tally_fields,
    valid_answers_query,
    user_field_to_point_sum_field,
)
from timApp.auth.accesshelper import get_doc_or_abort, AccessDenied
from timApp.document.docinfo import DocInfo
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext
from timApp.plugin.plugin import find_task_ids, CachedPluginFinder
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_view_access
from timApp.user.user import User, get_membership_end, get_membership_added
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException
from timApp.util.plugininfofield import PluginInfoField, get_plugininfo_field_values
from timApp.util.utils import widen_fields, get_alias, seq_to_str, fin_timezone

ALL_ANSWERED_WILDCARD = "*"


def chunks(l: list, n: int):
    for i in range(0, len(l), n):
        yield l[i : i + n]


tallyfield_re = re.compile(
    r"tally:((?P<doc>\d+)\.)?(?P<field>[a-zA-Z0-9öäåÖÄÅ_-]+)(?:.(?P<subfield>[a-zA-Z0-9öäåÖÄÅ_-]+))?(\[ *(?P<ds>[^\[\],]*) *, *(?P<de>[^\[\],]*) *\])?"
)


@attr.s(auto_attribs=True)
class TallyField:
    """In Jsrunner, represents the "tally:" type of field."""

    field: str
    subfield: str | None
    doc_id: int | None
    datetime_start: datetime | None
    datetime_end: datetime | None
    default_doc: DocInfo

    @property
    def effective_doc_id(self):
        return self.doc_id or self.default_doc.id

    @property
    def grouping_key(self):
        return f"{self.effective_doc_id}{self.datetime_start}{self.datetime_end}"

    @property
    def doc_and_field(self):
        return f"{self.effective_doc_id}.{self.field}"

    @staticmethod
    def try_parse(s: str, default_doc: DocInfo) -> Optional["TallyField"]:
        m = tallyfield_re.fullmatch(s)
        if not m:
            return None
        try:
            gds = m.group("ds")
            gde = m.group("de")
            ds, de = (
                dateutil.parser.parse(gds) if gds else None,
                dateutil.parser.parse(gde) if gde else None,
            )
        except (ValueError, OverflowError):
            return None
        if ds and ds.tzinfo is None:
            ds = fin_timezone.localize(ds)
        if de and de.tzinfo is None:
            de = fin_timezone.localize(de)
        doc = m.group("doc")
        return TallyField(
            field=m.group("field"),
            subfield=m.group("subfield"),
            datetime_start=ds,
            datetime_end=de,
            doc_id=int(doc) if doc else None,
            default_doc=default_doc,
        )


@unique
class MembershipFilter(Enum):
    All = "all"
    Current = "current"
    Deleted = "deleted"


member_filter_relation_map = {
    MembershipFilter.All: User.groups_dyn,
    MembershipFilter.Current: User.groups,
    MembershipFilter.Deleted: User.groups_inactive,
}

FieldValue = Union[str, float, None]
UserFields = dict[str, FieldValue]


class UserFieldObj(TypedDict):
    user: User
    fields: UserFields
    styles: Any


@dataclass
class RequestedGroups:
    groups: list[UserGroup]
    include_all_answered: bool = False

    @staticmethod
    def from_name_list(group_names: list[str]) -> "RequestedGroups":
        return RequestedGroups(
            groups=db.session.execute(
                select(UserGroup).filter(UserGroup.name.in_(group_names))
            )
            .scalars()
            .all(),
            include_all_answered=ALL_ANSWERED_WILDCARD in group_names,
        )


class GetFieldsAccess(Enum):
    RequireTeacher = 0
    AllowMaybeNonTeacher = 1
    AllowAlwaysNonTeacher = 2

    @staticmethod
    def from_bool(b: bool):
        return (
            GetFieldsAccess.AllowMaybeNonTeacher
            if b
            else GetFieldsAccess.RequireTeacher
        )


def _parse_field(
    field_text: str, doc: DocInfo
) -> TaskId | TallyField | PluginInfoField:
    task_id, err = TaskId.try_parse(
        field_text,
        require_doc_id=False,
        allow_block_hint=False,
        allow_type=False,
        allow_custom_field=True,
    )

    if task_id:
        task_id.doc_id = task_id.doc_id or doc.id
        return task_id

    tally_field = TallyField.try_parse(field_text, doc)
    if tally_field:
        return tally_field
    if field_text.startswith("tally:"):
        raise RouteException(f"Invalid tally field format: {field_text}")

    plugin_info_field = PluginInfoField.try_parse(field_text, doc)
    if plugin_info_field:
        return plugin_info_field
    if field_text.startswith("plugininfo:"):
        raise RouteException(f"Invalid plugininfo field format: {field_text}")

    if err:
        raise RouteException(f"Invalid field format: {field_text}: {err}")


def get_fields_and_users(
    u_fields: list[str],
    requested_groups: RequestedGroups,
    d: DocInfo,
    current_user: User,
    view_ctx: ViewContext,
    autoalias: bool = False,
    add_missing_fields: bool = False,
    access_option: GetFieldsAccess = GetFieldsAccess.RequireTeacher,
    member_filter_type: MembershipFilter = MembershipFilter.Current,
    user_filter=None,
) -> tuple[list[UserFieldObj], dict[str, str], list[str], list[UserGroup] | None]:
    """
    Return fielddata, aliases, field_names

    :param view_ctx: The view context.
    :param user_filter: Additional filter to use.
    :param member_filter_type: Whether to use all, current or deleted users in groups.
    :param u_fields: list of fields to be used
    :param requested_groups: requested user groups to be used
    :param d: default document
    :param current_user: current users, check his rights to fields
    :param autoalias: if true, give automatically from d1 same as would be from d1 = d1
    :param add_missing_fields: return estimated field even if it wasn't given previously
    :param access_option: option specifying who is allowed to access fields (non-teachers, teachers)
    :return: fielddata, aliases, field_names
    """
    allow_non_teacher = False
    if access_option == GetFieldsAccess.AllowAlwaysNonTeacher:
        allow_non_teacher = True
    elif access_option == GetFieldsAccess.AllowMaybeNonTeacher:
        allow_non_teacher = not requested_groups.include_all_answered
    needs_group_access_check = UserGroup.get_teachers_group() not in current_user.groups
    ugroups = []
    for group in requested_groups.groups:
        if needs_group_access_check and group.name != current_user.name:
            if not verify_group_view_access(group, current_user, require=False):
                # raise AccessDenied(f'Missing view access for group {group.name}')
                continue  # TODO: study how to give just warning from missing access, extra return string?
        ugroups.append(group)

    if (
        not ugroups and not requested_groups.include_all_answered
    ):  # if no access, give at least own group
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
        raise RouteException(f"Problem with field names: {u_fields}\n{e}")

    tasks_without_fields = []
    tally_fields: list[tuple[TallyField, str | None]] = []
    tasks_with_count_field = []
    plugin_info_fields: list[tuple[PluginInfoField, str | None]] = []

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
            raise RouteException(f"Invalid alias: {field}")
        if a == "":
            raise RouteException(f"Alias cannot be empty: {field}")

        field_obj = _parse_field(t, d)

        if isinstance(field_obj, TaskId):
            if field_obj.field is None:
                tasks_without_fields.append(field_obj)
            elif field_obj.field == "count":
                tasks_with_count_field.append(field_obj)
            task_ids.append(field_obj)

            task_id_map[field_obj.doc_task].append(field_obj)
            did = field_obj.doc_id
            alias_map_value = field_obj.extended_or_doc_task
        elif isinstance(field_obj, TallyField):
            did = field_obj.effective_doc_id
            alias_map_value = field_obj.doc_and_field
            tally_fields.append((field_obj, a))
        elif isinstance(field_obj, PluginInfoField):
            did = field_obj.doc_id
            alias_map_value = field_obj.doc_and_field
            plugin_info_fields.append((field_obj, a))
        else:
            raise Exception(f"Unhandled field type: {field_obj}")

        if a:
            alias_map[alias_map_value] = a
            if a in jsrunner_alias_map:
                raise RouteException(f"Duplicate alias {a} in fields attribute")
            jsrunner_alias_map[a] = alias_map_value

        if did in doc_map:
            continue
        dib = get_doc_or_abort(did, f"Document {did} not found")
        if not (current_user.has_teacher_access(dib) or allow_non_teacher):
            raise AccessDenied(f"Missing teacher access for document {dib.id}")
        elif dib.document.get_settings().get(
            "need_view_for_answers", False
        ) and not current_user.has_view_access(dib):
            raise AccessDenied("Sorry, you don't have permission to use this resource.")
        doc_map[did] = dib

    cpf = CachedPluginFinder(
        doc_map=doc_map,
        curr_user=UserContext.from_one_user(current_user),
        view_ctx=view_ctx,
    )
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

    group_id_set = {ug.id for ug in groups}
    group_filter = UserGroup.id.in_([ug.id for ug in groups])
    if user_filter is not None:
        group_filter = group_filter & user_filter
    join_relation = member_filter_relation_map[member_filter_type]

    user_ctx = UserContext.from_one_user(current_user)
    tally_field_values = get_tally_field_values(
        d,
        doc_map,
        group_filter if not requested_groups.include_all_answered else None,
        join_relation,
        tally_fields,
        view_ctx,
        user_ctx,
    )

    plugininfo_field_values = get_plugininfo_field_values(
        plugin_info_fields, d, doc_map, view_ctx, user_ctx
    )

    # FIXME: SQLAlchemy check behaviour
    sub = []
    # For some reason, with 7 or more fields, executing the following query is very slow in PostgreSQL 9.5.
    # That's why we split the list of task ids in chunks of size 6 and merge the results.
    # TODO: Investigate if this is still true for PostgreSQL 11.
    not_global_taskids = [t for t in task_ids if not t.is_global]
    for task_chunk in chunks(not_global_taskids, 6):
        q = valid_answers_query(task_chunk).join(User, Answer.users)
        if not requested_groups.include_all_answered:
            q = q.join(UserGroup, join_relation).filter(group_filter)
        elif user_filter is not None:
            # Ensure user filter gets applied even if group filter is skipped in include_all_answered
            q = q.filter(user_filter)
        sub += db.session.execute(
            q.group_by(Answer.task_id, User.id).with_only_columns(
                func.max(Answer.id), User.id
            )
        ).all()
    aid_uid_map = defaultdict(list)
    user_ids = set()
    for aid, uid in sub:
        aid_uid_map[aid].append(uid)
        user_ids.add(uid)

    q1 = select(User).join(UserGroup, join_relation).filter(group_filter)
    if requested_groups.include_all_answered:
        # if no group filter is given, attempt to get users that have valid answers only using the user
        # ids from previous query
        id_filter = User.id.in_(user_ids)
        # Ensure that user filter gets applied even if group filter was None
        if user_filter is not None:
            id_filter = id_filter & user_filter
        q2 = select(User).filter(id_filter)
        q = q1.with_only_columns(User.id).union(q2.with_only_columns(User.id))
        q = select(User).filter(User.id.in_(q))
    else:
        q = q1
    q = q.with_only_columns(User).order_by(User.id).options(lazyload(User.groups))
    if member_filter_type != MembershipFilter.Current:
        q = q.options(selectinload(User.memberships))
    users: list[User] = db.session.execute(q).scalars().all()
    user_map = {}
    for u in users:
        user_map[u.id] = u
    global_taskids = [t for t in task_ids if t.is_global]
    global_answer_ids = (
        db.session.execute(
            valid_answers_query(global_taskids)
            .group_by(Answer.task_id)
            .with_only_columns(func.max(Answer.id))
        )
        .scalars()
        .all()
    )
    answs = (
        db.session.execute(
            select(Answer).filter(
                Answer.id.in_(
                    itertools.chain((aid for aid, _ in sub), global_answer_ids)
                )
            )
        )
        .scalars()
        .all()
    )
    answers_with_users: list[tuple[int, Answer | None]] = []
    for a in answs:
        uids = aid_uid_map.get(a.id)
        if uids is not None:
            for uid in uids:
                answers_with_users.append((uid, a))
        else:
            # This is a global task, so add the answer for all users.
            for uid in user_map.keys():
                answers_with_users.append((uid, a))
    missing_users = {u.id for u in users} - {uid for uid, _ in answers_with_users}
    for mu in missing_users:
        answers_with_users.append((mu, None))
    answers_with_users.sort(key=lambda au: au[0])
    counts: dict[int, dict[str, int]] = {}
    if tasks_with_count_field:
        for u in users:
            counts[u.id] = {}
        cnt = func.count(Answer.id).label("cnt")
        answer_counts = db.session.execute(
            select(Answer)
            .filter(
                Answer.task_id.in_([tid.doc_task for tid in tasks_with_count_field])
            )
            .join(User, Answer.users)
            .filter(User.id.in_([u.id for u in users]))
            .group_by(User.id, Answer.task_id)
            .with_only_columns(User.id, Answer.task_id, cnt)
        ).all()
        for uid, taskid, count in answer_counts:
            counts[uid][taskid] = count

    last_user = None
    user_tasks = None
    user_fieldstyles = None
    user_index = -1
    res: list[UserFieldObj] = []
    for uid, a in answers_with_users:
        if last_user != uid:
            user_index += 1
            tally_values = tally_field_values.get(uid)
            user_tasks = {}
            if tally_values:
                user_tasks = user_tasks | {a: v for v, a in tally_values}
            if plugininfo_field_values:
                user_tasks = user_tasks | plugininfo_field_values
            if tasks_with_count_field:
                user_tasks = user_tasks | {
                    alias_map.get(
                        tid.extended_or_doc_task, tid.extended_or_doc_task
                    ): counts.get(uid).get(tid.doc_task, 0)
                    for tid in tasks_with_count_field
                }
            user_fieldstyles = {}
            user = users[user_index]
            assert user.id == uid
            obj = {"user": user, "fields": user_tasks, "styles": user_fieldstyles}
            res.append(obj)
            m_add = get_membership_added(user, group_id_set)
            m_end = (
                get_membership_end(user, group_id_set)
                if member_filter_type != MembershipFilter.Current
                else None
            )
            obj["groupinfo"] = {
                "membership_add": time.mktime(m_add.timetuple()) if m_add else None,
                "membership_end": time.mktime(m_end.timetuple()) if m_end else None,
            }
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
                    style = p.get("styles")
                if task.field == "points":
                    value = a.points
                elif task.field == "datetime":
                    value = time.mktime(a.answered_on.timetuple())
                elif task.field == "isodatetime":
                    value = datetime_isoformat(a.answered_on)
                elif task.field == "ALL":
                    value = p
                elif task.field == "count":
                    continue
                else:
                    if task.field:
                        try:
                            value = p.get(task.field)
                        except:
                            value = json.dumps(p)
                    else:
                        plug = cpf.find(task)
                        if plug:
                            content_field = plug.get_content_field_name()
                        else:
                            content_field = "c"
                        if isinstance(p, dict):
                            value = p.get(content_field)
                        else:
                            value = p
            user_tasks[
                alias_map.get(task.extended_or_doc_task, task.extended_or_doc_task)
            ] = value
            user_fieldstyles[
                alias_map.get(task.extended_or_doc_task, task.extended_or_doc_task)
            ] = style
    return (
        res,
        jsrunner_alias_map,
        [
            alias_map.get(ts.extended_or_doc_task, ts.extended_or_doc_task)
            for ts in task_ids
        ],
        groups,
    )


def get_tally_field_values(
    d: DocInfo,
    doc_map: dict[int, DocInfo],
    group_filter,
    join_relation,
    tally_fields: list[tuple[TallyField, str | None]],
    view_ctx: ViewContext,
    user_ctx: UserContext,
):
    tally_field_values: DefaultDict[int, list[tuple[float, str]]] = defaultdict(list)
    task_id_cache = {}
    field_groups = itertools.groupby(tally_fields, key=lambda f: f[0].grouping_key)
    for _, x in field_groups:  # type: str, list[tuple[TallyField, str | None]]
        fs = list(x)
        g = fs[0][0]
        doc = doc_map[g.doc_id].document if g.doc_id else d.document
        tids = task_id_cache.get(doc.doc_id)
        if tids is None:
            doc.insert_preamble_pars()
            pars = doc.get_dereferenced_paragraphs(view_ctx)
            tids = find_task_ids(pars, view_ctx, user_ctx, check_access=False)[0]
            task_id_cache[doc.doc_id] = tids
        ans_filter = true()
        if g.datetime_start:
            ans_filter = ans_filter & (Answer.answered_on >= g.datetime_start)
        if g.datetime_end:
            ans_filter = ans_filter & (Answer.answered_on < g.datetime_end)
        psr = doc.get_settings().point_sum_rule()
        pts = get_points_by_rule(
            rule=psr,
            task_ids=tids,
            user_ids=db.session.execute(
                select(User.id).join(UserGroup, join_relation).filter(group_filter)
            )
            .scalars()
            .all()
            if group_filter is not None
            else None,
            answer_filter=ans_filter,
            with_answer_time=True,
        )

        known_tally_fields = list(
            itertools.chain(basic_tally_fields, psr.get_groups(tids) if psr else [])
        )
        for field, _ in fs:
            if field.field not in known_tally_fields:
                raise RouteException(
                    f"Unknown tally field: {field.field} (full name: {field.doc_and_field}). "
                    f"Valid tally fields are: {seq_to_str(known_tally_fields)}."
                )
        for r in pts:
            u = r["user"]
            groups = r.get("groups", None)
            for field, alias in fs:
                # The value can be None if the user has not done any tasks with points, so we use another sentinel.
                value = r.get(field.field, missing)
                if value is missing:
                    value = groups[
                        field.field
                    ]  # The group should exist because the field was validated above.
                    value = (
                        value.get(normalize_tally_subfield(field.subfield), None)
                        if field.subfield
                        else value["total_sum"]
                    )
                tally_field_values[u.id].append((value, alias or field.doc_and_field))
    return tally_field_values


def normalize_tally_subfield(subfield: str) -> str:
    return user_field_to_point_sum_field.get(subfield, subfield)
