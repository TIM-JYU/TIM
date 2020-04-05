"""Answer-related routes."""
from datetime import datetime, timezone, timedelta
from typing import List, Set, Tuple

import dateutil.parser
import dateutil.relativedelta
from flask import request

from timApp.auth.sessioninfo import get_current_user_object
from timApp.plugin.taskid import TaskId
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import get_option
from timApp.util.utils import get_current_time


def task_ids_to_strlist(ids: List[TaskId]) -> List[str]:
    return [t.doc_task for t in ids]


def period_handling(task_ids: List[TaskId], doc_ids: Set[int], period: str) -> Tuple[datetime, datetime]:
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
        pf = last_answer_fetch.get(since_last_key)
        if pf is None:
            period_from = datetime.min.replace(tzinfo=timezone.utc)
        else:
            period_from = dateutil.parser.parse(pf)
        last_answer_fetch[since_last_key] = get_current_time().isoformat()
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
