import enum
import re
from typing import Dict, Union


class PointType(enum.Enum):
    task = 1
    velp = 2

class CountMethod(enum.Enum):
    latest = 1
    max = 2

class Group:

    def __init__(self, name: str, data: Union[str, Dict]) -> None:
        self.name = name
        if isinstance(data, str):
            self.matchers = {data}
            self.point_types = {PointType.task, PointType.velp}
            self.min_points = 0
            self.max_points = 1e100
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

    def check_match(self, task_id: str):
        try:
            return any(re.fullmatch(regex, task_id.split('.')[1]) is not None for regex in self.matchers)
        except re.error:
            return False


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
        
        scoreboard = data.get('scoreboard', {})
        self.scoreboard_groups = scoreboard.get('groups', None)
        try:
            self.scoreboard_count_method = CountMethod[str(scoreboard.get('count_method', 'latest')).lower()]
        except KeyError:
            # TODO: make user see this message somehow. Group's exception probably too
            raise Exception('Invalid scoreboard count_method.')
        
        self.total = data.get('total', None)
        self.hide = data.get('hide', None)
        self.sort = data.get('sort', True)
        self.count_all = data.get('count_all', False)

    def find_groups(self, task_id):
        for g in self.groups.values():
            if g.check_match(task_id):
                yield g.name
