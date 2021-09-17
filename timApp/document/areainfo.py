from dataclasses import dataclass
from typing import Optional


@dataclass
class AreaBoundary:
    name: str


@dataclass
class AreaStart(AreaBoundary):
    is_collapsed: Optional[bool]  # If None, not a collapsible area

    @property
    def area_class_str(self) -> str:
        return 'area collapsed' if self.is_collapsed else 'area'

    @property
    def collapse_state(self) -> str:
        return 'areaexpand' if self.is_collapsed else 'areacollapse'


@dataclass
class AreaEnd(AreaBoundary):
    pass
