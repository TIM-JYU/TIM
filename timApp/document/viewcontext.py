import re
from dataclasses import dataclass
from enum import Enum
from typing import Tuple, Any, Optional


class ViewRoute(Enum):
    Answers = 'answers'
    Lecture = 'lecture'
    ShowSlide = 'show_slide'
    Slide = 'slide'
    Teacher = 'teacher'
    Velp = 'velp'
    View = 'view'
    Review = 'review'
    Unknown = 'unknown'

    @property
    def teacher_or_see_answers(self) -> bool:
        return self in teacher_or_see_answers

    @property
    def is_review(self) -> bool:
        return self == ViewRoute.Review


teacher_or_see_answers = {ViewRoute.Teacher, ViewRoute.Answers}


viewmode_routes = {
    ViewRoute.Lecture,
    ViewRoute.ShowSlide,
    ViewRoute.Slide,
    ViewRoute.Velp,
    ViewRoute.View,
}


@dataclass(frozen=True, eq=True)
class ViewContext:
    route: ViewRoute
    preview: bool
    hide_names_requested: bool = False
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
