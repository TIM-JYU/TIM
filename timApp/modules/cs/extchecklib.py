from typing import List, Dict, Union, Optional
from dataclasses import field
from marshmallow_dataclass import dataclass

from loadable import Loadable

@dataclass
class AngularComponent(Loadable):
    template: str = field(default="")
    component: str = field(default="class {}")

@dataclass
class AngularModule(Loadable):
    components: Dict[str, AngularComponent] = field(default_factory=dict)
    entry: str = field(default="")

@dataclass
class DivContent(Loadable):
    classes: str = field(default="")
    content: str = field(default="")

@dataclass
class OutputContainer(Loadable):
    title: Optional[DivContent] = field(default=None)
    text: Optional[DivContent] = field(default=None)
    angular: Optional[AngularModule] = field(default=None)
    hide: bool = field(default=False) # whether to hide by default

@dataclass
class RunResult(Loadable):
    output_boxes: List[OutputContainer] = field(default_factory=list)
    penalties: Dict[str, Union[bool, str]] = field(default_factory=dict)
    points: float = field(default=0.0)
    max_points: Optional[float] = field(default=None)

    def penalize(self, key: str) -> bool:
        return self.penalties and self.penalties.get(key, False)
