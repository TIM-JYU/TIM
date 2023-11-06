"""Answer-related routes."""
from dataclasses import dataclass, field
from datetime import datetime, timezone, timedelta
from enum import Enum

import dateutil.parser
import dateutil.relativedelta

from timApp.auth.sessioninfo import get_current_user_object
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.util.utils import get_current_time


def task_ids_to_strlist(ids: list[TaskId]) -> set[str]:
    return set(t.doc_task for t in ids)


GLOBAL_SINCE_LAST_KEY = "*"


class PeriodOptions(Enum):
    WHENEVER = "whenever"
    DAY = "day"
    WEEK = "week"
    MONTH = "month"
    SINCE_LAST = "sincelast"
    OTHER = "other"


@dataclass
class AnswerPeriodOptions:
    period: PeriodOptions = field(
        default=PeriodOptions.WHENEVER, metadata={"by_value": True}
    )
    period_from: datetime | None = field(
        default=None, metadata={"data_key": "periodFrom"}
    )
    period_to: datetime | None = field(default=None, metadata={"data_key": "periodTo"})


def get_answer_period(
    task_ids: list[TaskId], doc_ids: set[int], options: AnswerPeriodOptions
) -> tuple[datetime, datetime]:
    """
    Returns start and end of a period for answer results.

    :param task_ids: Task ids containing the answers.
    :param doc_ids: Documents containing the answers.
    :param options:
    :return: Return "from"-period and "to"-period.
    """
    period_from = datetime.min.replace(tzinfo=timezone.utc)
    period_to = get_current_time()

    since_last_key = task_ids[0].doc_task if task_ids else GLOBAL_SINCE_LAST_KEY
    if len(task_ids) > 1:
        since_last_key = str(next(d for d in doc_ids))
        if len(doc_ids) > 1:
            since_last_key = GLOBAL_SINCE_LAST_KEY

        # Period from which to take results.
    match options.period:
        case PeriodOptions.WHENEVER:
            pass
        case PeriodOptions.SINCE_LAST:
            u = get_current_user_object()
            prefs = u.get_prefs()
            last_answer_fetch = prefs.last_answer_fetch
            pf = last_answer_fetch.get(since_last_key)
            if pf is None:
                period_from = datetime.min.replace(tzinfo=timezone.utc)
            else:
                period_from = dateutil.parser.parse(pf)
            last_answer_fetch[since_last_key] = get_current_time().isoformat()
            prefs.last_answer_fetch = last_answer_fetch
            u.set_prefs(prefs)
            db.session.commit()
        case PeriodOptions.DAY:
            period_from = period_to - timedelta(days=1)
        case PeriodOptions.WEEK:
            period_from = period_to - timedelta(weeks=1)
        case PeriodOptions.MONTH:
            period_from = period_to - dateutil.relativedelta.relativedelta(months=1)
        case PeriodOptions.OTHER:
            period_from = options.period_from or period_from
            period_to = options.period_to or period_to

    return period_from, period_to
