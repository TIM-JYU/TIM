from dataclasses import dataclass
from typing import List, Dict, Optional
from collections import OrderedDict

from timApp.answer.pointsumrule import PointCountMethod
from timApp.document.docinfo import DocInfo
from timApp.document.docsettings import DocSettings
from timApp.document.document import dereference_pars
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import default_view_ctx
from timApp.folder.folder import Folder
from timApp.plugin.plugin import find_task_ids, find_plugin_from_document, TaskNotFoundException


@dataclass
class TaskScoreInfo:
    taskName: str
    fragId: str
    points: float
    maxPoints: float


@dataclass
class DocScoreInfo:
    doc: DocInfo
    total: float
    maxTotal: float
    tasks: List[TaskScoreInfo]


def get_score_infos(
        folder: Folder,
        doc_paths: List[str],
        user_ctx: UserContext,
) -> List[DocScoreInfo]:
    total_table = OrderedDict()
    u = user_ctx.logged_user
    docs = folder.get_all_documents(
        relative_paths=doc_paths,
        filter_user=u,
    )

    def doc_sorter(d: DocInfo) -> int:
        rel_path = folder.relative_path(d)
        return doc_paths.index(rel_path)
    docs.sort(key=doc_sorter)

    for d in docs:
        d.document.insert_preamble_pars()

    user_context = UserContext.from_one_user(u)
    for d in docs:
        doc = d.document
        blocks = doc.get_paragraphs()
        blocks = dereference_pars(blocks, context_doc=doc, view_ctx=default_view_ctx)
        task_ids, _, _ = find_task_ids(blocks, default_view_ctx, user_ctx)
        if not task_ids:
            continue

        point_sum_rule = doc.get_settings().point_sum_rule()
        count_method = point_sum_rule.scoreboard.point_count_method if point_sum_rule else PointCountMethod.latest

        # cycle through all tasks in current document, resolving user's progress on each scored assignment
        point_dict: Dict[str, TaskScoreInfo] = {}
        for task_id in task_ids:
            try:
                plugin = find_plugin_from_document(doc, task_id, user_context, default_view_ctx)
            except TaskNotFoundException:
                continue

            max_points_str = plugin.max_points()
            if not max_points_str:
                continue
            try:
                max_points = float(max_points_str)
            except ValueError:
                # TODO: figure out something for when max_points is not convertible to float
                continue
            if max_points == 0:
                continue
            # TODO: point_sum_rule can also have max_points - maybe take it into account somehow.

            task = task_id.task_name
            if count_method == PointCountMethod.max:
                user_points = max((a.points for a in u.get_answers_for_task(task_id.doc_task) if a.points is not None), default=0)
            elif count_method == PointCountMethod.latest:
                latest_answer = u.get_answers_for_task(task_id.doc_task).first()
                user_points = latest_answer.points if latest_answer and latest_answer.points is not None else 0
            else:
                raise Exception(f'Unexpected count_method: {count_method}')

            # add current document to overall points list, using task_name as the key identifier
            # group tasks in the list by point_sum_rule groups
            # link takes to the first task in a group ("frag_id")
            groups = []
            included_groups = []
            include_groupless = True  # whether to include tasks without groups
            if point_sum_rule is not None:
                groups = list(point_sum_rule.find_groups(task_id.doc_task))
                included_groups = point_sum_rule.scoreboard.groups
                if not included_groups:
                    included_groups = groups
                # only include groupless if scoreboard_groups weren't specified or * is in there
                elif '*' not in included_groups:
                    include_groupless = False

            if groups:
                groups = [g for g in groups if g in included_groups]

                for grp in groups:
                    tps = point_dict.get(grp)
                    if tps:
                        tps.maxPoints += max_points
                        tps.points += user_points
                    else:
                        point_dict[grp] = TaskScoreInfo(grp, task, user_points, max_points)
            elif include_groupless:
                point_dict[task] = TaskScoreInfo(task, task, user_points, max_points)

        if not point_dict:
            continue

        tasks = list(point_dict.values())

        user_total = sum((t.points for t in tasks if t.points is not None))
        max_total = sum((t.maxPoints for t in tasks if t.maxPoints is not None))

        total_table[folder.relative_path(d)] = DocScoreInfo(d, user_total, max_total, tasks)

    return list(total_table.values())


def get_score_infos_if_enabled(
        doc_info: DocInfo,
        doc_settings: DocSettings,
        user_ctx: UserContext,
) -> Optional[List[DocScoreInfo]]:
    score_infos = None
    if user_ctx.logged_user.logged_in and doc_settings.show_scoreboard():
        scoreboard_docs = doc_settings.scoreboard_docs()
        if not scoreboard_docs:
            scoreboard_docs.append(doc_info.short_name)
        score_infos = get_score_infos(doc_info.parent, scoreboard_docs, user_ctx)
    return score_infos
