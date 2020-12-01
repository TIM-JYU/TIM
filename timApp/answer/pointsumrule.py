import enum
import re
from dataclasses import dataclass, field
from typing import Dict, Union, List, Generator

from marshmallow import ValidationError

from marshmallow_dataclass import class_schema


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

    def __init__(self, name: str, data: Union[str, Dict]) -> None:
        self.name = name
        if isinstance(data, str):
            self.matchers = {data}
            self.point_types = {PointType.task, PointType.velp}
            self.min_points = 0
            self.max_points = 1e100
            self.expl = "{0}: {1:.1f}"
            self.link = False
            self.linktext = None
        elif isinstance(data, dict):
            match_re = data.get('match', name)
            # match can be a single regex or a list of regexes
            if isinstance(match_re, str):
                self.matchers = {match_re}
            elif isinstance(match_re, list):
                self.matchers = set(match_re)
            else:
                raise Exception('Unknown type for match.')
            point_type = data.get('type', 'vt')
            self.point_types = set()
            if 'v' in point_type:
                self.point_types.add(PointType.velp)
            if 't' in point_type:
                self.point_types.add(PointType.task)
            self.min_points = data.get("min_points", 0)
            self.max_points = data.get("max_points", 1e100)
            self.expl = data.get("expl", "{0}: {1:.1f}")
            self.link = data.get("link", False)
            self.linktext = data.get("linktext", None)

    def check_match(self, task_id: str):
        try:
            return any(re.fullmatch(regex, task_id.split('.')[1]) is not None for regex in self.matchers)
        except re.error:
            return False


@dataclass
class ScoreboardOptions:
    groups: List[str] = field(default_factory=list)
    point_count_method: PointCountMethod = PointCountMethod.latest


ScoreboardOptionsSchema = class_schema(ScoreboardOptions)


class PointSumRule:

    def __init__(self, data: Dict) -> None:
        try:
            self.groups = dict((k, Group(k, v)) for k, v in data['groups'].items())
        except (AttributeError, KeyError):
            self.groups = {}
        try:
            self.count_type, self.count_amount = next(data['count'].items().__iter__())
        except (StopIteration, KeyError):
            self.count_type, self.count_amount = 'best', 9999

        self.scoreboard_error = None
        try:
            scoreboard = ScoreboardOptionsSchema().load(data.get('scoreboard', {}))
        except ValidationError as e:
            self.scoreboard_error = e
            scoreboard = ScoreboardOptions()

        self.scoreboard = scoreboard
        self.total = data.get('total', None)
        self.hide = data.get('hide', None)
        self.sort = data.get('sort', True)
        self.count_all = data.get('count_all', False)
        self.breaklines = data.get('breaklines', False)
        self.force = data.get('force', False)
        self.linktext = data.get('linktext', 'link')

    def find_groups(self, task_id: str) -> Generator[str, None, None]:
        for g in self.groups.values():
            if g.check_match(task_id):
                yield g.name
