from typing import List, Dict, Union, Optional
from dataclasses import field
from tim_common.marshmallow_dataclass import dataclass

from loadable import Loadable

@dataclass
class AngularComponent(Loadable):
    template: str = ""
    component: str = "class {}"

@dataclass
class AngularModule(Loadable):
    components: Dict[str, AngularComponent] = field(default_factory=dict)
    entry: str = ""

@dataclass
class DivContent(Loadable):
    classes: str = ""
    content: str = ""

@dataclass
class OutputContainer(Loadable):
    title: Optional[DivContent] = None
    text: Optional[DivContent] = None
    angular: Optional[AngularModule] = None
    html: Optional[DivContent] = None
    hide: bool = False # whether to hide by default

@dataclass
class RunResult(Loadable):
    output_boxes: List[OutputContainer] = field(default_factory=list)
    penalties: Dict[str, Union[bool, str]] = field(default_factory=dict)
    points: float = 0.0
    max_points: Optional[float] = None

    def penalize(self, key: str) -> bool:
        return self.penalties and self.penalties.get(key, False)
