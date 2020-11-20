import re
from dataclasses import dataclass
from enum import Enum
from typing import Tuple, Any, Optional


class ViewRoute(Enum):
    Answers = 'answers'
    Lecture = 'lecture'
    Slide = 'slide'
    Teacher = 'teacher'
    Velp = 'velp'
    View = 'view'
    Unknown = 'unknown'


viewmode_routes = {
    ViewRoute.Lecture,
    ViewRoute.Slide,
    ViewRoute.Velp,
    ViewRoute.View,
}


@dataclass(frozen=True)
class ViewContext:
    route: ViewRoute
    preview: bool
    urlmacros: Tuple[Tuple[str, str], ...] = ()

    @property
    def viewmode(self) -> bool:
        return self.route in viewmode_routes

    def isview(self, ret_val: bool, mode: Any = None) -> bool:
        if not isinstance(mode, str):
            if self.preview or self.route == ViewRoute.Unknown:
                return True
            if self.viewmode:
                return ret_val
            return not ret_val
        if re.match(mode, self.route.value):
            return ret_val
        return not ret_val


default_view_ctx = ViewContext(ViewRoute.View, False)


def viewroute_from_str(s: str) -> Optional[ViewRoute]:
    try:
        return ViewRoute(s)
    except ValueError:
        return None
