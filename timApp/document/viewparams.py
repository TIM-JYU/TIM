from dataclasses import dataclass
from typing import Optional

from marshmallow_dataclass import class_schema


@dataclass(frozen=True)
class ViewParams:
    """View route parameters that don't affect document rendering."""
    direct_link_timer: int = 15
    goto: Optional[str] = None
    login: bool = False
    nocache: bool = False
    wait_max: int = 0


ViewParamsSchema = class_schema(ViewParams)()
