import json
import re
from dataclasses import dataclass
from enum import Enum
from typing import Any, Optional, Tuple


class ViewRoute(Enum):
    Answers = "answers"
    Lecture = "lecture"
    ShowSlide = "show_slide"
    Slide = "slide"
    Teacher = "teacher"
    Velp = "velp"
    View = "view"
    Review = "review"
    Unknown = "unknown"

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

DEFAULT_VIEWMODE_TEMPLATE = "view_html"

viewmode_templates = {
    ViewRoute.ShowSlide: "show_slide",
    ViewRoute.Slide: "slide",
}


@dataclass(frozen=True)
class OriginInfo:
    doc_id: int
    par_id: str


# Mypy needs capital "Tuple" here.
UrlMacros = tuple[tuple[str, str], ...]
ExtraMacros = dict[str, object]


@dataclass(frozen=True)
class ViewContext:
    route: ViewRoute
    preview: bool
    partial: bool = False
    hide_names_requested: bool = False
    urlmacros: UrlMacros = ()
    extramacros: str = ""
    origin: OriginInfo | None = None

    @property
    def viewmode(self) -> bool:
        return self.route in viewmode_routes

    @property
    def url_params(self) -> UrlMacros:
        return self.urlmacros  # TODO urlmacros should be a subset of all params

    @property
    def extra_macros(self) -> ExtraMacros:
        if not self.extramacros:
            return {}
        em = json.loads(self.extramacros)
        return em  # TODO same tape for url and extra macros

    def get_url_param(self, key: str) -> str | None:
        for k, v in self.url_params:
            if k == key:
                return v
        return None

    @property
    def args(self) -> dict[str, str]:
        return {k: v for k, v in self.url_params}

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


def copy_of_default_view_ctx(extramacros: ExtraMacros) -> ViewContext:
    return ViewContext(ViewRoute.View, False, extramacros=json.dumps(extramacros))


def viewroute_from_str(s: str) -> ViewRoute | None:
    try:
        return ViewRoute(s)
    except ValueError:
        return None
