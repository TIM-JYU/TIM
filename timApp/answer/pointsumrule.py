import enum
import re
from dataclasses import dataclass, field
from typing import Generator

from marshmallow import ValidationError, EXCLUDE

from timApp.plugin.taskid import TaskId
from tim_common.marshmallow_dataclass import class_schema


class PointType(enum.Enum):
    task = 1
    velp = 2


class PointCountMethod(enum.Enum):
    """Point count method for scoreboard."""

    # Counts points by the latest answer per task.
    latest = 1

    # Counts points by the answer with the most points per task.
    max = 2


class Group:
    def __init__(self, name: str, data: str | dict) -> None:
        self.name = name
        if isinstance(data, str):
            self.matchers = {data}
            self.point_types = {PointType.task, PointType.velp}
            self.min_points: float = 0
            self.max_points: float = 1e100
            self.expl: str = "{0}: {1:.1f}"
            self.link: bool = False
            self.linktext: str | None = None
        elif isinstance(data, dict):
            match_re = data.get("match", name)
            # match can be a single regex or a list of regexes
            if isinstance(match_re, str):
                self.matchers = {match_re}
            elif isinstance(match_re, list):
                # TODO: Display error if regexes are invalid => e.g. wrong type
                self.matchers = set(str(r) for r in match_re)
            else:
                raise Exception("Unknown type for match.")
            point_type = data.get("type", "vt")
            self.point_types = set()
            if "v" in point_type:
                self.point_types.add(PointType.velp)
            if "t" in point_type:
                self.point_types.add(PointType.task)
            self.min_points = data.get("min_points", 0)
            self.max_points = data.get("max_points", 1e100)
            self.expl = data.get("expl", "{0}: {1:.1f}")
            self.link = data.get("link", False)
            self.linktext = data.get("linktext", None)
            self.preformat_points = data.get("preformat_points", False)

    def check_match(self, task_id: str) -> bool:
        try:
            return any(
                re.fullmatch(regex, task_id.split(".")[1]) is not None
                for regex in self.matchers
            )
        except re.error:
            return False


@dataclass(frozen=True)
class ScoreboardOptions:
    groups: list[str] = field(default_factory=list)


@dataclass(frozen=True)
class CountModel:
    best: int | None = None
    worst: int | None = None


# TODO: Add all pointsumrule fields under this.
@dataclass(frozen=True)
class PointSumRuleModel:
    count: CountModel = CountModel(best=9999)
    scoreboard: ScoreboardOptions = ScoreboardOptions()
    include_groupless: bool = False
    point_count_method: PointCountMethod = PointCountMethod.latest


PointSumRuleSchema = class_schema(PointSumRuleModel)


class PointSumRule:
    def __init__(self, data: dict) -> None:
        try:
            self.groups = {k: Group(k, v) for k, v in data["groups"].items()}
        except (AttributeError, KeyError):
            self.groups = {}

        self.scoreboard_error = None
        try:
            pr: PointSumRuleModel = PointSumRuleSchema().load(data, unknown=EXCLUDE)
        except ValidationError as e:
            self.scoreboard_error = e
            pr = PointSumRuleModel()
        if pr.count.best is not None:
            self.count_type, self.count_amount = "best", pr.count.best
        elif pr.count.worst is not None:
            self.count_type, self.count_amount = "worst", pr.count.worst

        self.scoreboard = pr.scoreboard
        self.hide_total_points = data.get("hide_total_points", False)
        self.hide_total_tasks = data.get("hide_total_tasks", False)
        self.include_groupless = pr.include_groupless
        self.point_count_method = pr.point_count_method
        self.total = data.get("total", None)
        self.hide = data.get("hide", None)
        self.sort = data.get("sort", True)
        self.count_all = data.get("count_all", False)
        self.breaklines = data.get("breaklines", False)
        self.force = data.get("force", False)
        self.linktext = data.get("linktext", "link")

    def find_groups(self, task_id: str) -> Generator[str, None, None]:
        for g in self.groups.values():
            if g.check_match(task_id):
                yield g.name

    def get_groups(self, task_ids: list[TaskId] | None = None) -> dict[str, Group]:
        if task_ids is None:
            return self.groups
        groups = dict(self.groups)
        if self.include_groupless:
            for id in task_ids:
                if all(not g.check_match(id.doc_task) for g in groups.values()):
                    groups[id.task_name] = Group(id.task_name, id.doc_task)
        return groups
