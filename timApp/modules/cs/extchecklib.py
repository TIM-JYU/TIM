from dataclasses import field
from typing import Union, Optional

from loadable import Loadable
from tim_common.marshmallow_dataclass import dataclass


@dataclass
class AngularComponent(Loadable):
    template: str = ""
    component: str = "class {}"


@dataclass
class AngularModule(Loadable):
    components: dict[str, AngularComponent] = field(default_factory=dict)
    entry: str = ""


@dataclass
class DivContent(Loadable):
    classes: str = ""
    content: str = ""


@dataclass
class OutputContainer(Loadable):
    title: DivContent | None = None
    text: DivContent | None = None
    angular: AngularModule | None = None
    html: DivContent | None = None
    hide: bool = False  # whether to hide by default


@dataclass
class RunResult(Loadable):
    output_boxes: list[OutputContainer] = field(default_factory=list)
    penalties: dict[str, bool | str] = field(default_factory=dict)
    points: float = 0.0
    max_points: float | None = None

    def penalize(self, key: str) -> bool:
        return self.penalties and self.penalties.get(key, False)
