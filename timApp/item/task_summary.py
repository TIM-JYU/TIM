from dataclasses import dataclass

from timApp.answer.answers import AnswerCountRule, get_points_by_rule
from timApp.document.docinfo import DocInfo
from timApp.document.document import dereference_pars
from timApp.document.preloadoption import PreloadOption
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.plugin.plugin import find_task_ids
from timApp.timdb.exceptions import PreambleException
from timApp.user.user import User


@dataclass
class TaskInfo:
    """Holds computed task summary information for a user on a document."""

    total_points: float | None = None
    tasks_done: int | None = None
    total_tasks: int = 0
    show: bool = False
    groups: dict | None = None
    breaklines: bool = False
    hide_total_points: bool = False
    hide_total_tasks: bool = False


def compute_task_info(
    doc_info: DocInfo,
    current_user: User,
    task_ids: list | None = None,
    valid_answers_only: bool | None = None,
) -> TaskInfo:
    """Compute task summary info for the given user and document.

    :param doc_info: The document to compute task info for.
    :param current_user: The user whose task info to compute.
    :param task_ids: Pre-computed task IDs. If None, they will be derived from the document.
    :param valid_answers_only: Override for valid-answers-only setting. If None, uses doc settings.
    :return: A TaskInfo with the computed values.
    """
    doc_settings = doc_info.document.get_settings()
    points_sum_rule = doc_settings.point_sum_rule()

    if task_ids is None:
        view_ctx = default_view_ctx
        doc = doc_info.document
        doc.preload_option = PreloadOption.all
        pars = doc.get_paragraphs()
        try:
            pars = doc.insert_preamble_pars() + pars
        except PreambleException:
            pass
        pars = dereference_pars(pars, context_doc=doc, view_ctx=view_ctx)
        task_ids, _, _ = find_task_ids(
            pars,
            view_ctx,
            UserContext.from_one_user(current_user),
            check_access=False,
        )

    if points_sum_rule and not points_sum_rule.count_all:
        total_tasks = len(points_sum_rule.groups)
    else:
        total_tasks = len(task_ids)

    show_valid_only = (
        valid_answers_only
        if valid_answers_only is not None
        else doc_settings.show_valid_answers_only()
    )
    answer_count_rule = (
        AnswerCountRule.OnlyValid
        if show_valid_only
        else AnswerCountRule.ValidThenInvalid
    )

    info_result = TaskInfo(total_tasks=total_tasks)

    if not current_user.logged_in:
        return info_result

    info = get_points_by_rule(
        points_sum_rule,
        task_ids,
        [current_user.id],
        force_user=current_user,
        count_rule=answer_count_rule,
    )
    if info:
        info_result.total_points = info[0]["total_points"]
        info_result.tasks_done = info[0]["task_count"]
        groups = info[0].get("groups")
        if isinstance(groups, dict) or groups is None:
            info_result.groups = groups
        info_result.show = (
            info_result.tasks_done or 0
        ) > 0 or info_result.total_points != 0
        if points_sum_rule:
            info_result.breaklines = points_sum_rule.breaklines
            info_result.show = info_result.show or points_sum_rule.force
            info_result.hide_total_points = points_sum_rule.hide_total_points
            info_result.hide_total_tasks = points_sum_rule.hide_total_tasks

    return info_result
