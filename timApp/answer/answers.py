""""""
import hashlib
import json
from collections import defaultdict, OrderedDict
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import (
    Any,
    Generator,
    TypeVar,
    SupportsRound,
    DefaultDict,
    TypedDict,
    ItemsView,
    cast,
    Sequence,
)

# This ref exits in bs4 but doesn't seem to be correctly exported
# noinspection PyUnresolvedReferences
from bs4 import UnicodeDammit
from flask import current_app
from sqlalchemy import (
    func,
    Numeric,
    Float,
    true,
    case,
    select,
    Result,
    Label,
)
from sqlalchemy.dialects.postgresql import aggregate_order_by
from sqlalchemy.orm import defaultload, selectinload, contains_eager
from sqlalchemy.sql import Select, Subquery
from sqlalchemy.sql.elements import OperatorExpression

from timApp.answer.answer import Answer
from timApp.answer.answer_models import AnswerTag, UserAnswer
from timApp.answer.pointsumrule import PointSumRule, PointType, Group
from timApp.document.viewcontext import OriginInfo
from timApp.plugin.plugintype import PluginType, PluginTypeLazy, PluginTypeBase
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db, run_sql
from timApp.upload.upload import get_pluginupload
from timApp.user.user import Consent, User
from timApp.user.usergroup import UserGroup
from timApp.util.answerutil import (
    task_ids_to_strlist,
    AnswerPeriodOptions,
)
from timApp.util.logger import log_warning
from timApp.velp.annotation_model import Annotation
from tim_common.utils import round_float_error


@dataclass
class ExistingAnswersInfo:
    latest_answer: Answer | None
    count: int


def get_answers_query(task_id: TaskId, users: list[User], only_valid: bool) -> Select:
    stmt = select(Answer).filter_by(task_id=task_id.doc_task)
    if only_valid:
        stmt = stmt.filter_by(valid=True)
    if not task_id.is_global:
        stmt = (
            stmt.join(User, Answer.users)
            .filter(User.id.in_([u.id for u in users]))
            .group_by(Answer.id)
            .having(
                (func.array_agg(aggregate_order_by(User.id, User.id)))  # type: ignore[no-untyped-call]
                == sorted(u.id for u in users)
            )
            .with_only_columns(Answer.id)
        )
        stmt = select(Answer).filter(Answer.id.in_(stmt))
    stmt = stmt.order_by(Answer.id.desc())
    return stmt


def get_latest_answers_query(
    task_id: TaskId, users: list[User], only_valid: bool
) -> Select:
    stmt = select(Answer).filter_by(task_id=task_id.doc_task)
    if only_valid:
        stmt = stmt.filter_by(valid=True)
    stmt_sub = (
        stmt.join(User, Answer.users)
        .filter(User.id.in_([u.id for u in users]))
        .group_by(User.id)
        .with_only_columns(func.max(Answer.id).label("aid"), User.id.label("uid"))
        .subquery()
    )
    return select(Answer).join(stmt_sub, Answer.id == stmt_sub.c.aid)


def is_redundant_answer(
    content: str,
    existing_answers: ExistingAnswersInfo,
    ptype: PluginTypeBase | None,
    valid: bool,
) -> bool:
    la = existing_answers.latest_answer
    is_redundant = la and (la.content == content and la.valid == valid)
    if is_redundant:
        return True
    if (
        existing_answers.count == 0
        and ptype
        and ptype.get_type() == "rbfield"
        and json.loads(content)["c"] == "0"
    ):
        return True
    return False


class TooLargeAnswerException(Exception):
    pass


_datetime_in_testing: datetime | None = None


# Only used in tests to set a fixed datetime
def set_test_datetime(dt: datetime | None) -> None:
    global _datetime_in_testing
    _datetime_in_testing = dt


def save_answer(
    users: list[User],
    task_id: TaskId,
    content: Any,
    points: float | None,
    tags: list[str] | None = None,
    valid: bool = True,
    points_given_by: int | None = None,
    force_save: bool = False,
    saver: User | None = None,
    plugintype: PluginTypeLazy | None = None,
    max_content_len: int | None = None,
    origin: OriginInfo | None = None,
    answered_on: datetime | None = None,
    overwrite_existing: bool = False,
) -> Answer | None:
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
    :param origin: If known, the document from which the answer was sent.
    :param answered_on: If specified, overrides the date when the answer was saved. If None, uses the current date.
    :param overwrite_existing: If specified, overwrites the existing answer with the new content if it exists.

    """
    # Never save points with round-off errors (could be caused by any computation)
    points = round_float_error(points)
    content_str = json.dumps(content)
    content_len = len(content_str)
    if max_content_len and content_len > max_content_len:
        log_warning(f"Task {task_id.doc_task}: too large answer ({content_len})")
        raise TooLargeAnswerException(
            f"Answer is too large (size is {content_len} but maximum is {max_content_len})."
        )
    if tags is None:
        tags = []
    answerinfo = get_existing_answers_info(users, task_id, False)
    if (
        is_redundant_answer(content_str, answerinfo, plugintype, valid)
        and not force_save
    ):
        if answerinfo.latest_answer:
            a = answerinfo.latest_answer
            a.points = points
            a.last_points_modifier = points_given_by
        return None
    if overwrite_existing and answerinfo.latest_answer:
        a = answerinfo.latest_answer
        a.content = content_str
        a.points = points
        a.last_points_modifier = points_given_by
        a.valid = valid
        a.answered_on = answered_on or datetime.now()
        return None

    a = Answer(
        task_id=task_id.doc_task,
        content=content_str,
        points=points,
        valid=valid,
        last_points_modifier=points_given_by,
        origin_doc_id=origin.doc_id if origin else None,
        plugin_type=plugintype.resolve() if plugintype else None,
    )
    if _datetime_in_testing:
        a.answered_on = _datetime_in_testing
    if answered_on:
        a.answered_on = answered_on
    db.session.add(a)

    for u in users:
        a.users.append(u)

    for tag in tags:
        at = AnswerTag(tag=tag)
        db.session.add(at)
    if saver:
        a.saver = saver
    db.session.flush()
    return a


class AgeOptions(Enum):
    MIN = "min"
    MAX = "max"
    ALL = "all"


class ValidityOptions(Enum):
    ALL = "all"
    VALID = "1"
    INVALID = "0"


class NameOptions(Enum):
    USERNAME = "username"
    BOTH = "both"
    ANON = "anonymous"
    PSEUDO = "pseudonym"


class SortOptions(Enum):
    USERNAME = "username"
    TASK = "task"
    DATE_ASCENDING = "date"
    DATE_DESCENDING = "date_desc"


class FormatOptions(Enum):
    JSON = "json"
    TEXT = "text"


class AnswerPrintOptions(Enum):
    ALL = "all"
    ANSWERS = "answers"
    ANSWERS_NO_LINE = "answersnoline"
    HEADER = "header"
    KORPPI = "korppi"


@dataclass
class AllAnswersOptions(AnswerPeriodOptions):
    group: list[str] | None = field(default=None, metadata={"list_type": "delimited"})
    groups: list[str] | None = field(default=None, metadata={"list_type": "delimited"})
    age: AgeOptions = field(default=AgeOptions.MAX, metadata={"by_value": True})
    valid: ValidityOptions = field(
        default=ValidityOptions.VALID, metadata={"by_value": True}
    )
    name: NameOptions = field(default=NameOptions.BOTH, metadata={"by_value": True})
    sort: SortOptions = field(default=SortOptions.TASK, metadata={"by_value": True})
    format: FormatOptions = field(
        default=FormatOptions.TEXT, metadata={"by_value": True}
    )
    consent: Consent | None = field(
        default=None, metadata={"by_value": True, "data_key": "consentOpt"}
    )
    print: AnswerPrintOptions = field(
        default=AnswerPrintOptions.ALL, metadata={"by_value": True}
    )
    salt: str | None = None
    salt_len: int = field(default=32, metadata={"data_key": "saltLen"})
    include_inactive_memberships: bool = field(
        default=False, metadata={"data_key": "includeInactiveMemberships"}
    )

    def __post_init__(self) -> None:
        if self.group:
            self.groups = self.groups + self.group if self.groups else self.group


def get_all_answers(
    task_ids: list[TaskId],
    options: AllAnswersOptions,
) -> list[str] | list[dict]:
    """Gets all answers to the specified tasks.

    :param task_ids: The ids of the tasks to get answers for.
    :param options: The options for getting and printing the answers.
    """
    print_header = options.print in (AnswerPrintOptions.ALL, AnswerPrintOptions.HEADER)
    print_answers = options.print in (
        AnswerPrintOptions.ALL,
        AnswerPrintOptions.ANSWERS,
        AnswerPrintOptions.ANSWERS_NO_LINE,
    )
    anon_name = options.name in (NameOptions.ANON, NameOptions.PSEUDO)

    if options.period_from is None or options.period_to is None:
        raise ValueError("Answer period must be specified.")

    stmt = get_all_answer_initial_query(
        options.period_from,
        options.period_to,
        task_ids,
        options.valid,
        options.groups,
        options.include_inactive_memberships,
    )

    stmt = stmt.options(defaultload(Answer.users).lazyload(User.groups))

    if options.consent is not None:
        stmt = stmt.filter_by(consent=options.consent)

    counts: Label[bool] | Label[int]
    match options.age:
        case AgeOptions.MIN:
            minmax = func.min(Answer.id).label("minmax")
            counts = func.count(Answer.answered_on).label("count")
        case AgeOptions.MAX:
            minmax = func.max(Answer.id).label("minmax")
            counts = func.count(Answer.answered_on).label("count")
        case AgeOptions.ALL:
            minmax = Answer.id.label("minmax")
            counts = Answer.valid.label("count")
        case _:
            raise ValueError(f"Invalid age option: {options.age}")

    # stmt = stmt.add_columns(minmax, counts)
    if options.age != AgeOptions.ALL:
        stmt = stmt.group_by(Answer.task_id, User.id)
    stmt = stmt.with_only_columns(minmax, counts)
    sub_stmt: Subquery = stmt.subquery()
    stmt = (
        select(Answer)
        .join(sub_stmt, Answer.id == sub_stmt.c.minmax)
        .join(User, Answer.users)
    )
    stmt = stmt.outerjoin(PluginType).options(contains_eager(Answer.plugin_type))
    match options.sort:
        case SortOptions.USERNAME:
            stmt = stmt.order_by(User.name, Answer.task_id, Answer.answered_on)
        case SortOptions.TASK:
            stmt = stmt.order_by(Answer.task_id, User.name, Answer.answered_on)
        case SortOptions.DATE_ASCENDING:
            stmt = stmt.order_by(Answer.answered_on, Answer.task_id, User.name)
        case SortOptions.DATE_DESCENDING:
            stmt = stmt.order_by(Answer.answered_on.desc(), Answer.task_id, User.name)

    stmt = stmt.with_only_columns(Answer, User, sub_stmt.c.count)
    result = []
    result_json = []

    lf = "\n"
    if options.print == AnswerPrintOptions.ANSWERS_NO_LINE:
        lf = ""

    qq: Result[tuple[Answer, User, int]] = run_sql(stmt)
    cnt = 0
    hidden_user_names: dict[str, str] = {}

    hashes = set()

    def hasher(x: int) -> str:
        nonlocal cnt
        cnt += 1
        return str(cnt)

    if options.name == NameOptions.PSEUDO and options.salt is not None:

        def hasher(x: int) -> str:
            assert options.salt is not None
            shake = hashlib.shake_256()
            shake.update(options.salt.encode("utf-8"))
            shake.update(str(x).encode("utf-8"))
            hash_result = shake.hexdigest(options.salt_len)
            if hash_result in hashes:
                return f"{hash_result}-DUPLICATE"
            hashes.add(hash_result)
            return hash_result

    for a, u, n in qq:
        points = str(a.points)
        if points == "None":
            points = ""
        name = u.name
        ns = str(int(n))  # n may be a boolean, so convert to int (0/1) first
        if anon_name:
            name = hidden_user_names.get(u.name, None)
            if not name:
                name = f"user_{hasher(u.id)}"
                hidden_user_names[u.name] = name
        try:
            line = json.loads(a.content)
        except json.JSONDecodeError:
            line = a.content
        header = "; ".join(
            [
                name,
                str(a.origin_doc_id),
                a.task_id,
                a.plugin_type.type if a.plugin_type else "",
                str(a.answered_on),
                ns,
                points,
            ]
        )
        answ = json.dumps(line, ensure_ascii=False)
        if isinstance(line, dict):  # maybe csPlugin?
            files = line.get("uploadedFiles")
            if isinstance(files, list):
                if len(files) == 1:
                    p = files[0]["path"]
                    prefix = "/uploads/"
                    if p.startswith(prefix):
                        p = p[len(prefix) :]
                    mt, pu = get_pluginupload(p)
                    if mt == "text/plain":
                        try:
                            answ = pu.data.decode()
                        except UnicodeDecodeError:
                            answ = UnicodeDammit(pu.data).unicode_markup
                    else:
                        answ = "ERROR: Uploaded file is binary; cannot show content."
                else:
                    answ = f"ERROR: There are more than 1 file uploads ({len(files)}) in this answer; cannot show content."
            elif "usercode" in line:
                answ = str(line.get("usercode", "-"))
            else:
                if "points" in line:  # empty csPlugin answer
                    answ = ""

        match options.format:
            case FormatOptions.TEXT:
                res = ""
                if options.name == NameOptions.BOTH:
                    header = str(u.real_name) + "; " + header
                if print_header:
                    res = header
                if print_answers:
                    res += lf + answ
                if options.print == AnswerPrintOptions.KORPPI:
                    res = name + ";"
                    taskid = a.task_id
                    i = taskid.find(".")
                    if i >= 0:
                        taskid = taskid[i + 1 :]
                    res += taskid + ";" + answ.replace("\n", "\\n")

                result.append(res)
            case FormatOptions.JSON:
                user_json = u.to_json() if print_header else {}
                user_json["name"] = name
                if options.name != NameOptions.BOTH:
                    user_json.pop("real_name", None)
                if anon_name:
                    user_json.pop("id", None)
                    user_json.pop("student_id", None)
                    user_json.pop("email", None)
                answer_json = a.to_json()

                if not print_answers:
                    answer_json.pop("content", None)
                    answer_json.pop("origin_doc_id", None)
                if not print_header:
                    answer_json.pop("id", None)
                    answer_json.pop("answered_on", None)
                    answer_json.pop("valid", None)
                    answer_json.pop("last_points_modifier", None)
                    answer_json.pop("points", None)
                    answer_json.pop("task_id", None)
                    answer_json.pop("origin_doc_id", None)
                    answer_json.pop("plugin", None)

                result_json_item: dict[str, Any] = {
                    "answer": answer_json,
                }
                if print_header:
                    result_json_item |= {
                        "count": int(n),
                        "user": user_json,
                    }
                if print_answers:
                    result_json_item |= {"resolved_content": answ}

                result_json.append(result_json_item)
    if options.format == FormatOptions.TEXT:
        return result
    else:
        return result_json


def get_all_answer_initial_query(
    period_from: datetime,
    period_to: datetime,
    task_ids: list[TaskId],
    valid: ValidityOptions,
    groups: list[str] | None = None,
    include_expired_members: bool = False,
) -> Select:
    stmt = (
        select(Answer)
        .filter((period_from <= Answer.answered_on) & (Answer.answered_on < period_to))
        .filter(Answer.task_id.in_(task_ids_to_strlist(task_ids)))
    )
    match valid:
        case ValidityOptions.ALL:
            pass
        case ValidityOptions.INVALID:
            stmt = stmt.filter_by(valid=False)
        case ValidityOptions.VALID:
            stmt = stmt.filter_by(valid=True)
    stmt = stmt.join(User, Answer.users)
    if groups:
        stmt = stmt.join(
            UserGroup, User.groups_dyn if include_expired_members else User.groups
        ).filter(UserGroup.name.in_(groups))
    return stmt


def get_existing_answers_info(
    users: list[User], task_id: TaskId, only_valid: bool
) -> ExistingAnswersInfo:
    stmt = get_answers_query(task_id, users, only_valid)
    latest = run_sql(stmt.limit(1)).scalars().first()
    count = db.session.scalar(select(func.count()).select_from(stmt.subquery()))
    return ExistingAnswersInfo(latest_answer=latest, count=count)


basic_tally_fields = [
    "total_points",
    "velp_points",
    "task_points",
    "task_count",
    "velped_task_count",
    "first_answer_on",
    "last_answer_on",
    "answer_duration",
]
user_field_to_point_sum_field = {
    "total_points": "total_sum",
    "velp_points": "velp_sum",
}


def valid_answers_query(task_ids: list[TaskId], valid: bool | None = True) -> Select:
    return select(Answer).filter(valid_taskid_filter(task_ids, valid))


def valid_taskid_filter(
    task_ids: list[TaskId], valid: bool | None = True
) -> OperatorExpression[bool]:
    res: OperatorExpression[bool] = Answer.task_id.in_(task_ids_to_strlist(task_ids))
    if valid is not None:
        res = res & (Answer.valid == valid)
    return res


class UserTaskEntry(TypedDict):
    user: User
    task_count: int
    velped_task_count: int
    total_points: float | None
    task_points: float | None
    velp_points: float | None
    task_id: str
    doc_id: str
    first_answer_on: datetime | None
    last_answer_on: datetime | None
    answer_duration: timedelta | None


def task_entry_for_user(user: User) -> "UserTaskEntry":
    return UserTaskEntry(
        user=user,
        task_count=0,
        velped_task_count=0,
        total_points=None,
        task_points=None,
        velp_points=None,
        task_id="",
        doc_id="",
        first_answer_on=None,
        last_answer_on=None,
        answer_duration=None,
    )


def update_entry_totals(entry: UserTaskEntry) -> None:
    if entry["first_answer_on"] and entry["last_answer_on"]:
        entry["answer_duration"] = entry["last_answer_on"] - entry["first_answer_on"]

    if entry["task_points"] is not None and entry["velp_points"] is not None:
        entry["total_points"] = round(entry["task_points"] + entry["velp_points"], 4)
    elif entry["task_points"] is not None:
        entry["total_points"] = round(entry["task_points"], 4)
    elif entry["velp_points"] is not None:
        entry["total_points"] = round(entry["velp_points"], 4)

    entry["task_points"] = (
        round(entry["task_points"], 4) if entry["task_points"] is not None else None
    )
    entry["velp_points"] = (
        round(entry["velp_points"], 4) if entry["velp_points"] is not None else None
    )


class AnswerCountRule(Enum):
    """
    Rule for including and counting tasks.
    """

    OnlyValid = 1
    """Only count valid answer."""
    ValidThenInvalid = 2
    """Count valid answers. If the user has no valid answer, then count invalid tasks."""
    Any = 3
    """Count any answer, whichever is the latest."""


@dataclass(slots=True)
class UserAnswerInfo:
    answered_on_min: datetime | None = None
    answered_on_max: datetime | None = None
    answer_id: int = -1
    points: float | None = None
    velp_points: float | None = None
    velped: bool = False

    def update_entry(self, entry: UserTaskEntry) -> None:
        if self.points is not None:
            if entry["task_points"] is None:
                entry["task_points"] = 0.0
            assert entry["task_points"] is not None
            entry["task_points"] += self.points
        if self.velp_points is not None:
            if entry["velp_points"] is None:
                entry["velp_points"] = 0.0
            assert entry["velp_points"] is not None
            entry["velp_points"] += self.velp_points
        if self.answer_id >= 0:
            entry["task_count"] += 1
        if self.velped:
            entry["velped_task_count"] += 1
        if self.answered_on_min and (
            entry["first_answer_on"] is None
            or self.answered_on_min < entry["first_answer_on"]
        ):
            entry["first_answer_on"] = self.answered_on_min
        if self.answered_on_max and (
            entry["last_answer_on"] is None
            or self.answered_on_max > entry["last_answer_on"]
        ):
            entry["last_answer_on"] = self.answered_on_max


@dataclass(slots=True)
class SingleAnswerInfo:
    answer_id: int
    points: float | None


def get_users_for_tasks_py(
    task_ids: list[TaskId],
    user_ids: list[int] | None = None,
    group_by_user: bool = True,
    group_by_doc: bool = False,
    answer_filter: Any | None = None,
    with_answer_time: bool = False,
    count_rule: AnswerCountRule = AnswerCountRule.OnlyValid,
) -> list[UserTaskEntry]:
    if not task_ids:
        return []

    # Step 1: Collect all answers for the specified tasks and users

    if answer_filter is None:
        answer_filter = true()

    userid_filter: Any = true()
    if user_ids is not None:
        userid_filter = UserAnswer.user_id.in_(user_ids)

    user_answers_query = (
        select(Answer)
        .filter(answer_filter)
        .filter(Answer.task_id.in_(task_ids_to_strlist(task_ids)))
        .join(UserAnswer, UserAnswer.answer_id == Answer.id)
        .filter(userid_filter)
        .with_only_columns(
            Answer.id,
            Answer.task_id,
            Answer.points,
            Answer.valid,
            Answer.answered_on,
            UserAnswer.user_id,
        )
    )

    user_answers: Result[tuple[int, str, float | None, bool, datetime, int]] = run_sql(
        user_answers_query
    )
    user_answers_by_id: dict[int, dict[str, UserAnswerInfo]] = defaultdict(
        lambda: defaultdict(UserAnswerInfo)
    )
    answer_to_task_id: dict[int, str] = {}

    # Step 2: Collect the latest answer and latest valid answer for each user and task
    # Also we start precomputing answer times if they are enabled

    latest_answers_per_user: dict[
        tuple[int, str], tuple[SingleAnswerInfo | None, SingleAnswerInfo | None]
    ] = defaultdict(lambda: (None, None))

    for answer_id, task_id, points, valid, answered_on, user_id in user_answers:
        info = user_answers_by_id[user_id][task_id]
        answer_to_task_id[answer_id] = task_id
        latest_answer, latest_valid_answer = latest_answers_per_user[(user_id, task_id)]

        if latest_answer is None or answer_id > latest_answer.answer_id:
            if latest_answer is None:
                latest_answer = SingleAnswerInfo(answer_id, points)
            else:
                latest_answer.answer_id = answer_id
                latest_answer.points = points

        if valid and (
            latest_valid_answer is None or answer_id > latest_valid_answer.answer_id
        ):
            if latest_valid_answer is None:
                latest_valid_answer = SingleAnswerInfo(answer_id, points)
            else:
                latest_valid_answer.answer_id = answer_id
                latest_valid_answer.points = points

        latest_answers_per_user[(user_id, task_id)] = (
            latest_answer,
            latest_valid_answer,
        )

        if with_answer_time and (
            info.answered_on_max is None or answered_on > info.answered_on_max
        ):
            info.answered_on_max = answered_on
        if with_answer_time and (
            info.answered_on_min is None or answered_on < info.answered_on_min
        ):
            info.answered_on_min = answered_on

    # Step 3: Select the latest answer for each user and task according to the count rule

    answer_info_by_answer_id: dict[int, list[UserAnswerInfo]] = defaultdict(list)

    for (user_id, task_id), (
        latest_answer,
        latest_valid_answer,
    ) in latest_answers_per_user.items():
        match count_rule:
            case AnswerCountRule.OnlyValid:
                selected_answer = latest_valid_answer
            case AnswerCountRule.ValidThenInvalid:
                selected_answer = latest_valid_answer or latest_answer
            case AnswerCountRule.Any:
                selected_answer = latest_answer
        if selected_answer:
            # If the answer was selected, assign the points to the user and task
            info = user_answers_by_id[user_id][task_id]
            info.answer_id = selected_answer.answer_id
            info.points = selected_answer.points
            answer_info_by_answer_id[selected_answer.answer_id].append(info)
        else:
            # If no answer was selected, remove the task from the dict
            # If the user has no tasks left, remove the user so that they are not tallied
            del user_answers_by_id[user_id][task_id]
            if len(user_answers_by_id[user_id]) == 0:
                del user_answers_by_id[user_id]

    # Step 4: Select annotations for the selected answers and add the points to the user and task
    annotations_query = (
        select(Annotation)
        .filter_by(valid_until=None)
        .filter(
            Annotation.answer_id.in_(
                [
                    i.answer_id
                    for infos in user_answers_by_id.values()
                    for i in infos.values()
                    if i.answer_id >= 0
                ]
            )
        )
        .with_only_columns(Annotation.answer_id, Annotation.points)
    )

    annotations: Result[tuple[int, float | None]] = run_sql(annotations_query)
    for answer_id, points in annotations:
        infos = answer_info_by_answer_id[answer_id]
        if not points:
            continue
        for info in infos:
            if info.velp_points is None:
                info.velp_points = 0.0
            info.velp_points += points
            info.velped = True

    # Step 5: Collect the user infos of all users

    if user_ids:
        userid_filter = User.id.in_(user_ids)
    else:
        userid_filter = User.id.in_(user_answers_by_id.keys())

    users_query = select(User).filter(userid_filter).order_by(User.real_name)
    if current_app.config["LOAD_STUDENT_IDS_IN_TEACHER"]:
        users_query = users_query.options(selectinload("uniquecodes"))

    # Step 6: Aggregate the results based on the different grouping options

    def collect_by_user() -> list[UserTaskEntry]:
        result: list[UserTaskEntry] = []

        for user in run_sql(users_query).scalars():
            infos = user_answers_by_id.get(user.id)
            if not infos:
                continue

            entry = task_entry_for_user(user)
            for info in infos.values():
                info.update_entry(entry)

            update_entry_totals(entry)
            result.append(entry)

        return result

    def collect_by_doc() -> list[UserTaskEntry]:
        result: list[UserTaskEntry] = []

        user_answers_by_doc: dict[int, dict[str, list[UserAnswerInfo]]] = defaultdict(
            lambda: defaultdict(list)
        )

        for user_id, infos in user_answers_by_id.items():
            for task_id, info in infos.items():
                doc_id = task_id.split(".", maxsplit=1)[0]
                user_answers_by_doc[user_id][doc_id].append(info)

        for user in run_sql(users_query).scalars():
            entry_per_doc_id: dict[str, UserTaskEntry] = defaultdict(
                lambda: task_entry_for_user(user)
            )

            user_infos = user_answers_by_doc.get(user.id)
            if user_infos:
                for doc_id, user_doc_infos in user_infos.items():
                    for info in user_doc_infos:
                        info.update_entry(entry_per_doc_id[doc_id])

            for doc_id, entry in entry_per_doc_id.items():
                update_entry_totals(entry)
                entry["doc_id"] = doc_id
                result.append(entry)

        return result

    def collect_by_task() -> list[UserTaskEntry]:
        result: list[UserTaskEntry] = []

        for user in run_sql(users_query).scalars():
            infos = user_answers_by_id.get(user.id)

            if not infos:
                continue

            for task_id, info in infos.items():
                entry = task_entry_for_user(user)
                info.update_entry(entry)
                update_entry_totals(entry)
                entry["task_id"] = task_id
                result.append(entry)

        return result

    if group_by_doc:
        return collect_by_doc()
    if group_by_user:
        return collect_by_user()
    return collect_by_task()


def get_users_for_tasks(
    task_ids: list[TaskId],
    user_ids: list[int] | None = None,
    group_by_user: bool = True,
    group_by_doc: bool = False,
    answer_filter: Any | None = None,
    with_answer_time: bool = False,
    count_rule: AnswerCountRule = AnswerCountRule.OnlyValid,
) -> list[UserTaskEntry]:
    # FIXME: Fix the get_users_for_tasks_sql version and call it instead. See the PERF comment below for details.
    return get_users_for_tasks_py(
        task_ids,
        user_ids,
        group_by_user,
        group_by_doc,
        answer_filter,
        with_answer_time,
        count_rule,
    )


# PERF: Right now this single-query version is *very* slow.
#   The primary reason is related to the deeply nested joins that force the query planner to use many nested loops
#   without reusing data. The current query also uses multiple groupings with complex computations which further slows
#   down the query.
#   As a temporary measure, we use the get_users_for_tasks_py version instead which relies on three separate queries
#   and offloads computation to Python.
#   Before returning to the query version, the following changes should be made:
#     - Replace single query with CTEs (Common Table Expressions) to reuse data.
#     - Replace the in_ with ARRAY and ANY constructs in PostgreSQL,
#       as they are faster for large task_id and user lists.
#   Additionally, the following major (possibly breaking) changes should be considered:
#     - Consider using a materialized view for the query. However, this will require running
#       updates on the view periodically, which increases speed at the cost of data freshness.
#     - Consider adding an aggregate table that contains the per-user tally on each answer.
def get_users_for_tasks_sql(
    task_ids: list[TaskId],
    user_ids: list[int] | None = None,
    group_by_user: bool = True,
    group_by_doc: bool = False,
    answer_filter: Any | None = None,
    with_answer_time: bool = False,
    count_rule: AnswerCountRule = AnswerCountRule.OnlyValid,
) -> list[UserTaskEntry]:
    if not task_ids:
        return []
    subquery_annotantions = (
        select(Annotation)
        .filter_by(valid_until=None)
        .group_by(Annotation.answer_id)
        .with_only_columns(
            Annotation.answer_id.label("annotation_answer_id"),
            func.sum(Annotation.points).label("velp_points"),
        )
        .subquery()
    )
    subquery_answers = select(
        Answer.id, Answer.points, Answer.answered_on, Answer.valid
    ).subquery()
    if answer_filter is None:
        answer_filter = true()
    time_labels = (
        [
            func.min(Answer.answered_on).label("answered_on_min"),
            func.max(Answer.answered_on).label("answered_on_max"),
        ]
        if with_answer_time
        else []
    )
    subquery_user_answers = (
        valid_answers_query(
            task_ids, True if count_rule == AnswerCountRule.OnlyValid else None
        )
        .filter(answer_filter)
        .join(UserAnswer, UserAnswer.answer_id == Answer.id)
        .group_by(UserAnswer.user_id, Answer.task_id)
        .with_only_columns(
            Answer.task_id,
            UserAnswer.user_id.label("uid"),
            func.max(Answer.id).filter(Answer.valid == True).label("aid_valid"),  # type: ignore[no-untyped-call]
            func.max(Answer.id).label("aid_any"),
            *time_labels,
        )
        .subquery()
    )

    answer_select_condition: Any = true()
    annotation_select_condition: Any = true()
    match count_rule:
        case AnswerCountRule.OnlyValid:
            answer_select_condition = (subquery_user_answers.c.aid_valid != None) & (
                subquery_user_answers.c.aid_valid == subquery_answers.c.id
            )
            annotation_select_condition = (
                subquery_annotantions.c.annotation_answer_id
                == subquery_user_answers.c.aid_valid
            )
        case AnswerCountRule.ValidThenInvalid:
            answer_select_condition = (
                (subquery_user_answers.c.aid_valid != None)
                & (subquery_user_answers.c.aid_valid == subquery_answers.c.id)
            ) | (
                (subquery_user_answers.c.aid_valid == None)
                & (subquery_user_answers.c.aid_any == subquery_answers.c.id)
            )
            annotation_select_condition = (
                subquery_annotantions.c.annotation_answer_id
                == subquery_user_answers.c.aid_valid
            )
        case AnswerCountRule.Any:
            answer_select_condition = (
                subquery_user_answers.c.aid_any == subquery_answers.c.id
            )
            annotation_select_condition = (
                subquery_annotantions.c.annotation_answer_id
                == subquery_user_answers.c.aid_any
            )

    sub_joined = (
        select(subquery_user_answers, subquery_answers, subquery_annotantions)
        .outerjoin(
            subquery_answers,
            answer_select_condition,
        )
        .outerjoin(
            subquery_annotantions,
            annotation_select_condition,
        )
        .subquery()
    )

    sub_joined_answer_join_condition: Any = true()
    match count_rule:
        case AnswerCountRule.OnlyValid:
            sub_joined_answer_join_condition = (sub_joined.c.aid_valid != None) & (
                sub_joined.c.aid_valid == UserAnswer.answer_id
            )
        case AnswerCountRule.ValidThenInvalid:
            sub_joined_answer_join_condition = (
                (sub_joined.c.aid_valid != None)
                & (sub_joined.c.aid_valid == UserAnswer.answer_id)
            ) | (
                (sub_joined.c.aid_valid == None)
                & (sub_joined.c.aid_any == UserAnswer.answer_id)
            )
        case AnswerCountRule.Any:
            sub_joined_answer_join_condition = (
                sub_joined.c.aid_any == UserAnswer.answer_id
            )

    main_stmt = (
        select(User)
        .join(UserAnswer, UserAnswer.user_id == User.id)
        .join(
            sub_joined,
            sub_joined_answer_join_condition & (User.id == sub_joined.c.uid),
        )
    )
    group_by_cols = []
    cols = []
    if not group_by_user:
        min_task_id = func.min(sub_joined.c.task_id).label("task_id")
        group_by_cols.append(sub_joined.c.task_id)
        cols.append(min_task_id)
    if group_by_doc:
        doc_id = func.substring(sub_joined.c.task_id, r"(\d+)\..+").label("doc_id")
        group_by_cols.append(doc_id)
        cols.append(doc_id)
    if user_ids is not None:
        main_stmt = main_stmt.filter(User.id.in_(user_ids))
    if current_app.config["LOAD_STUDENT_IDS_IN_TEACHER"]:
        main_stmt = main_stmt.options(selectinload("uniquecodes"))
    main_stmt = main_stmt.group_by(User.id, *group_by_cols)

    def get_points(subquery_field: Any) -> Any:
        match count_rule:
            # When counting only valid answers or both, then only sum of the valid counts
            case AnswerCountRule.OnlyValid | AnswerCountRule.ValidThenInvalid:
                return case((sub_joined.c.valid == True, subquery_field), else_=0)
            case AnswerCountRule.Any:
                return subquery_field

    # prevents error:
    # column "usergroup_1.id" must appear in the GROUP BY clause or be used in an aggregate function
    main_stmt = main_stmt.options(selectinload(User.groups))
    task_sum = (
        func.round(
            func.sum(get_points(sub_joined.c.points)).cast(Numeric),
            4,
        )
        .cast(Float)
        .label("task_points")
    )
    velp_sum = (
        func.round(
            func.sum(get_points(sub_joined.c.velp_points)).cast(Numeric),
            4,
        )
        .cast(Float)
        .label("velp_points")
    )
    if with_answer_time:
        answered_on_min = func.min(sub_joined.c.answered_on_min).label(
            "first_answer_on"
        )
        answered_on_max = func.max(sub_joined.c.answered_on_max).label("last_answer_on")
        time_cols = [
            answered_on_min,
            answered_on_max,
            (answered_on_max - answered_on_min).label("answer_duration"),
        ]
    else:
        time_cols = []

    main_stmt = main_stmt.with_only_columns(
        User,
        func.count(sub_joined.c.task_id).label("task_count"),
        task_sum,
        velp_sum,
        # TODO: The following does not work because PostgreSQL evaluates a+b==null if a==null or b==null
        #  We want a+b to be null only if BOTH are null. For now, the summing is done in Python.
        # func.round((task_sum + velp_sum).cast(Numeric), 4).cast(Float).label('total_points'),
        func.count(sub_joined.c.annotation_answer_id).label("velped_task_count"),
        *time_cols,
        *cols,
    ).order_by(User.real_name)

    def g() -> Generator[UserTaskEntry, None, None]:
        for r in run_sql(main_stmt):
            # noinspection PyProtectedMember
            d = r._asdict()
            d["user"] = d.pop("User")
            task = d["task_points"]
            velp = d["velp_points"]
            if task is not None and velp is not None:
                tot = task + velp
            elif task is not None:
                tot = task
            else:
                tot = velp
            d["total_points"] = tot
            yield cast(UserTaskEntry, d)

    result = list(g())
    return result


T = TypeVar("T", bound=SupportsRound[Any])


def sum_and_round(generator: Generator[T, None, None], digits: int = 2) -> T | None:
    list_to_sum = list(generator)
    if not list_to_sum:
        return None
    return round(sum(list_to_sum), digits)  # type: ignore


def round_if_not_none(num: T | None, digits: int = 2) -> T | None:
    if num is None:
        return None
    return round(num, digits)


class SumFields(TypedDict):
    task_sum: float | None
    velp_sum: float | None
    total_sum: float | None


class CountFields(TypedDict):
    task_count: int
    velped_task_count: int


class DateFields(TypedDict):
    first_answer_on: datetime | None
    last_answer_on: datetime | None
    answer_duration: timedelta | None


class UserPointGroup(SumFields, DateFields, CountFields):
    tasks: list[UserTaskEntry]


class UserPointInfo(SumFields):
    groups: DefaultDict[str, UserPointGroup]


class ResultGroup(SumFields, DateFields, CountFields):
    text: str
    link: bool
    linktext: str


class UserPoints(TypedDict):
    total_points: float | None
    task_points: float | None
    velp_points: float | None
    task_count: int
    velped_task_count: int
    groups: dict[str, ResultGroup]
    user: User
    first_answer_on: datetime | None
    last_answer_on: datetime | None
    answer_duration: timedelta | None


def get_points_by_rule(
    rule: PointSumRule | None,
    task_ids: list[TaskId],
    user_ids: list[int] | None = None,
    answer_filter: Any | None = None,
    force_user: User | None = None,
    with_answer_time: bool = False,
    count_rule: AnswerCountRule = AnswerCountRule.OnlyValid,
) -> (
    list[UserPoints] | list[UserTaskEntry]
):  # TODO: Would be better to return always same kind of list.
    """Computes the point sum from given tasks according to the given point rule.

    :param force_user: Whether to force at least one result user if the result would be empty otherwise.
    :param answer_filter: Optional additional filter for answers.
    :param rule: The points rule.
    :param task_ids: The list of task ids to consider.
    :param user_ids: The list of users for which to compute the sum.
    :param with_answer_time: Whether to include the answer time data (last answer time, first answer time) in the result.
    :param count_rule: Rule for counting answers.

    :return: The computed result.

    """
    if not rule:
        return get_users_for_tasks(
            task_ids,
            user_ids,
            answer_filter=answer_filter,
            count_rule=count_rule,
        )
    tasks_users = get_users_for_tasks(
        task_ids,
        user_ids,
        group_by_user=False,
        answer_filter=answer_filter,
        with_answer_time=with_answer_time,
        count_rule=count_rule,
    )
    result: DefaultDict[int, UserPointInfo] = defaultdict(
        lambda: {
            "groups": defaultdict(
                lambda: {
                    "tasks": [],
                    "task_sum": None,
                    "velp_sum": None,
                    "total_sum": None,
                    "task_count": 0,
                    "velped_task_count": 0,
                    "first_answer_on": None,
                    "last_answer_on": None,
                    "answer_duration": None,
                }
            ),
            "task_sum": None,
            "velp_sum": None,
            "total_sum": None,
        }
    )
    task_counts: dict[int, int] = {}
    user_map = {}
    if not tasks_users and rule.force and force_user:
        for t in task_ids:
            tasks_users.append(
                UserTaskEntry(
                    user=force_user,
                    task_count=0,
                    velped_task_count=0,
                    velp_points=None,
                    total_points=None,
                    task_points=None,
                    task_id=t.doc_task,
                    doc_id=str(t.doc_id or 0),
                    first_answer_on=None,
                    last_answer_on=None,
                    answer_duration=None,
                )
            )
    rule_groups = dict(rule.groups)
    for tu in tasks_users:
        u = tu["user"]
        uid = u.id
        user_map[uid] = u
        if rule.count_all:  # TODO: would this info already been somewhere cheaper?
            c = task_counts.get(uid, 0)
            c += 1
            task_counts[uid] = c

        group_names = list(rule.find_groups(tu["task_id"]))
        if not group_names and rule.include_groupless:
            gname = TaskId.parse(tu["task_id"]).task_name
            group_names = [gname]
            rule_groups[gname] = Group(gname, gname)

        for grp in group_names:
            result[uid]["groups"][grp]["tasks"].append(tu)
    for user_id, task_groups in result.items():
        groups = task_groups["groups"]
        groupsums = []
        for groupname, group in groups.items():
            task_sum = None
            velp_sum = None
            gr = rule_groups[groupname]
            if PointType.task in gr.point_types:
                task_sum = sum_and_round(
                    t["task_points"]
                    for t in group["tasks"]
                    if t["task_points"] is not None
                )
            if PointType.velp in gr.point_types:
                velp_sum = sum_and_round(
                    t["velp_points"]
                    for t in group["tasks"]
                    if t["velp_points"] is not None
                )
            group["velped_task_count"] = sum(
                1 for t in group["tasks"] if t["velped_task_count"] > 0
            )
            group["task_count"] = len(group["tasks"])
            if task_sum is not None and velp_sum is not None:
                total_sum: float | None = task_sum + velp_sum
            elif task_sum is not None:
                total_sum = task_sum
            elif velp_sum is not None:
                total_sum = velp_sum
            else:
                total_sum = None
            total_sum = (
                min(max(total_sum, gr.min_points), gr.max_points)
                if total_sum is not None
                else None
            )
            group["task_sum"] = task_sum
            group["velp_sum"] = velp_sum
            group["total_sum"] = total_sum

            def valid_dates(date_attr: str) -> Generator[datetime, None, None]:
                for tt in group["tasks"]:
                    d = tt.get(date_attr, None)
                    if d is not None and isinstance(d, datetime):
                        yield d

            first_answer_on = min(valid_dates("first_answer_on"), default=None)
            last_answer_on = max(valid_dates("last_answer_on"), default=None)
            answer_duration = (
                last_answer_on - first_answer_on
                if last_answer_on and first_answer_on
                else None
            )
            group["last_answer_on"] = last_answer_on
            group["first_answer_on"] = first_answer_on
            group["answer_duration"] = answer_duration
            groupsums.append((task_sum, velp_sum, total_sum))
        groupsums = sorted(
            groupsums,
            reverse=rule.count_type == "best",
            key=lambda x: (x[2] is not None, x[2]),
        )
        if rule.total:
            s = groupsums[
                0
            ]  # TODO: find indices from total, total is a list of names to count
            task_groups["task_sum"] = round_if_not_none(s[0])
            task_groups["velp_sum"] = round_if_not_none(s[1])
            task_groups["total_sum"] = round_if_not_none(s[2])
        else:
            task_groups["task_sum"] = sum_and_round(
                s[0] for s in groupsums[0 : rule.count_amount] if s[0] is not None
            )
            task_groups["velp_sum"] = sum_and_round(
                s[1] for s in groupsums[0 : rule.count_amount] if s[1] is not None
            )
            task_groups["total_sum"] = sum_and_round(
                s[2] for s in groupsums[0 : rule.count_amount] if s[2] is not None
            )
    return flatten_points_result(rule, rule_groups, result, task_counts, user_map)


def flatten_points_result(
    rule: PointSumRule,
    rule_groups: dict[str, Group],
    result: DefaultDict[int, UserPointInfo],
    task_counts: dict[int, int],
    user_map: dict[int, User],
) -> list[UserPoints]:
    result_list = []
    hide_list = rule.hide
    for user_id, task_groups in result.items():
        first_answer_on = min(
            (
                t["first_answer_on"]
                for t in task_groups["groups"].values()
                if t["first_answer_on"] is not None
            ),
            default=None,
        )
        last_answer_on = max(
            (
                t["last_answer_on"]
                for t in task_groups["groups"].values()
                if t["last_answer_on"] is not None
            ),
            default=None,
        )
        answer_duration = (
            last_answer_on - first_answer_on
            if last_answer_on and first_answer_on
            else None
        )
        row = UserPoints(
            total_points=task_groups["total_sum"],
            task_points=task_groups["task_sum"],
            velp_points=task_groups["velp_sum"],
            task_count=task_counts.get(user_id, 0)
            if rule.count_all
            else sum(
                1
                for t in task_groups["groups"].values()
                if sum(x["task_count"] for x in t["tasks"]) > 0
            ),
            velped_task_count=sum(
                1 for t in task_groups["groups"].values() if t["velped_task_count"] > 0
            ),
            groups=OrderedDict(),
            user=user_map[user_id],
            first_answer_on=first_answer_on,
            last_answer_on=last_answer_on,
            answer_duration=answer_duration,
        )

        if rule.sort:
            rulegroups: ItemsView[str, Group] | list[tuple[str, Group]] = sorted(
                rule_groups.items()
            )
        else:
            rulegroups = rule_groups.items()
        for groupname, rg in rulegroups:
            if hide_list and groupname in hide_list:
                continue
            gr = task_groups["groups"].get(groupname)
            if gr:
                task_sum = gr["task_sum"]
                velp_sum = gr["velp_sum"]
                total_sum = gr["total_sum"]
                first_answer_on = gr["first_answer_on"]
                last_answer_on = gr["last_answer_on"]
                answer_duration = gr["answer_duration"]
                task_count = gr["task_count"]
                velped_task_count = gr["velped_task_count"]
            else:
                task_sum = None
                velp_sum = None
                total_sum = None
                first_answer_on = None
                last_answer_on = None
                answer_duration = None
                task_count = 0
                velped_task_count = 0
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
            row["groups"][groupname] = {
                "task_sum": task_sum,
                "velp_sum": velp_sum,
                "total_sum": total_sum,
                "text": text,
                "link": link,
                "linktext": linktext,
                "first_answer_on": first_answer_on,
                "last_answer_on": last_answer_on,
                "answer_duration": answer_duration,
                "task_count": task_count,
                "velped_task_count": velped_task_count,
            }
        result_list.append(row)
    return result_list


def add_missing_users_from_groups(result: list, usergroups: list[UserGroup]) -> list:
    users = set(u for ug in usergroups for u in ug.users)
    existing_users = set()
    for d in result:
        existing_users.add(d["user"])

    missing = users - existing_users

    for d in missing:
        result.append(
            {
                "task_count": 0,
                "task_points": None,
                "velp_points": 0.0,
                "total_points": None,
                "velped_task_count": 0,
                "user": d,
                "id": d.id,
                "name": d.name,
                "real_name": d.real_name,
                "email": d.email,
            }
        )

    # {'task_count': 1, 'task_points': None, 'velp_points': 0.0, 'total_points': None, 'velped_task_count': 0,
    # 'user': <User 6>, 'id': 6, 'name': 'testiuser@testi.fi', 'real_name': 'Testi User', 'email': 'testiuser@testi.fi'}

    return result


def get_global_answers(parsed_task_ids: dict[str, TaskId]) -> Sequence[Answer]:
    sq2 = (
        select(Answer)
        .filter(
            Answer.task_id.in_(
                [tid.doc_task for tid in parsed_task_ids.values() if tid.is_global]
            )
        )
        .group_by(Answer.task_id)
        .with_only_columns(func.max(Answer.id).label("aid"))
        .subquery()
    )
    global_datas = select(Answer).join(sq2, Answer.id == sq2.c.aid)
    return run_sql(global_datas).scalars().all()
