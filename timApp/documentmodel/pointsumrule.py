from typing import Dict, Union
import re

import enum


class PointType(enum.Enum):
    task = 1
    velp = 2


class Group:
    def __init__(self, name: str, data: Union[str, Dict]):
        self.name = name
        if isinstance(data, str):
            self.matchers = {data}
            self.point_types = {PointType.task, PointType.velp}
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

    def check_match(self, task_id: str):
        try:
            return any(re.fullmatch(regex, task_id.split('.')[1]) is not None for regex in self.matchers)
        except re.error:
            return False


class PointSumRule:
    def __init__(self, data: Dict):
        try:
            self.groups = dict((k, Group(k, v)) for k, v in data['groups'].items())
        except (AttributeError, KeyError):
            self.groups = {}
        try:
            self.count_type, self.count_amount = next(data['count'].items().__iter__())
        except (StopIteration, KeyError):
            self.count_type, self.count_amount = 'best', 9999

    def find_group(self, task_id):
        for g in self.groups.values():
            if g.check_match(task_id):
                return g.name
        return None
