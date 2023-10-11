import itertools
import re
from collections import defaultdict
from dataclasses import dataclass
from typing import Optional, Iterable

from timApp.answer.pointsumrule import PointSumRule
from timApp.document.docinfo import DocInfo
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext
from timApp.plugin.plugin import find_task_ids
from timApp.plugin.taskid import TaskId

PLUGININFO_FIELDS = {"count", "task_names", "task_ids"}
plugininfo_re = re.compile(
    r"plugininfo:((?P<doc>\d+)\.)?(?P<field>[a-zA-Z0-9öäåÖÄÅ_-]+)(?:.(?P<subfield>[a-zA-Z0-9öäåÖÄÅ_-]+))?"
)
ALL_POINT_SUM_GROUP = "ALL"


@dataclass
class PluginInfoField:
    doc_id: int
    point_sum_rule_group: str | None
    info_field: str

    @property
    def effective_point_sum_rule_group(self) -> str:
        return self.point_sum_rule_group or ALL_POINT_SUM_GROUP

    @property
    def doc_and_field(self) -> str:
        return f"{self.doc_id}.{self.effective_point_sum_rule_group}.{self.info_field}"

    @staticmethod
    def try_parse(s: str, default_doc: DocInfo) -> Optional["PluginInfoField"]:
        m = plugininfo_re.fullmatch(s)
        if not m:
            return None
        doc = m.group("doc")
        field = m.group("field")
        subfield = m.group("subfield")

        match (field, subfield):
            case (_, None):  # Only field is specified, assume it as a field
                point_sum_rule_group = None
                info_field = field
            case (_, _):  # Both field and subfield are specified
                point_sum_rule_group = field
                info_field = subfield
            case _:  # Field is invalid
                return None

        if info_field not in PLUGININFO_FIELDS:
            return None

        return PluginInfoField(
            doc_id=int(doc) if doc else default_doc.id,
            point_sum_rule_group=point_sum_rule_group,
            info_field=info_field,
        )


def group_task_ids_by_rule(
    task_ids: list[TaskId],
    rule: PointSumRule | None,
) -> dict[str, list[TaskId]]:
    result = defaultdict(list)
    result[ALL_POINT_SUM_GROUP] = list(task_ids)

    if rule is None:
        return result

    for g in rule.get_groups(task_ids).values():
        for tid in task_ids:
            if g.check_match(tid.doc_task):
                result[g.name].append(tid)
    return result


def get_plugininfo_field_values(
    plugininfo_fields: list[tuple[PluginInfoField, str | None]],
    doc: DocInfo,
    doc_map: dict[int, DocInfo],
    view_ctx: ViewContext,
    user_ctx: UserContext,
) -> dict[str, float]:
    fields_by_doc: Iterable[
        tuple[int, Iterable[tuple[PluginInfoField, str | None]]]
    ] = itertools.groupby(plugininfo_fields, key=lambda f: f[0].doc_id)

    result = {}

    for doc_id, fields in fields_by_doc:
        fs = list(fields)
        d = doc_obj.document if (doc_obj := doc_map.get(doc_id)) else doc.document
        d.insert_preamble_pars()
        pars = d.get_dereferenced_paragraphs(view_ctx)
        tids = find_task_ids(pars, view_ctx, user_ctx, check_access=False)[0]
        psr = d.get_settings().point_sum_rule()
        grouped_tasks = group_task_ids_by_rule(tids, psr)
        for field, alias in fs:
            matched_tids = grouped_tasks.get(field.effective_point_sum_rule_group, [])

            match field.info_field:
                case "count":
                    val = float(len(matched_tids))
                case "task_names":
                    val = [tid.task_name for tid in matched_tids]
                case "task_ids":
                    val = [tid.doc_task for tid in matched_tids]
                case _:
                    val = 0.0

            result[alias or field.doc_and_field] = val

    return result
