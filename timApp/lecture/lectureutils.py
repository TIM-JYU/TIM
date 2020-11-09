from dataclasses import dataclass

from timApp.auth.sessioninfo import get_current_user_id, get_current_user_object
from timApp.lecture.lecture import Lecture
from timApp.util.flask.requesthelper import RouteException


def is_lecturer_of(l: Lecture) -> bool:
    return l.lecturer == get_current_user_id()


def verify_is_lecturer(l: Lecture) -> None:
    u = get_current_user_object()
    if not is_lecturer_of(l) and not u.is_admin:
        raise RouteException('Only lecturer can perform this action.')


@dataclass
class CurrentLectureInfo:
    in_lecture: bool
    is_lecturer: bool


def get_current_lecture_info() -> CurrentLectureInfo:
    lectures = get_current_user_object().lectures.all()
    is_in_lecture = bool(lectures and lectures[0].is_running)
    if is_in_lecture:
        return CurrentLectureInfo(
            in_lecture=is_in_lecture,
            is_lecturer=is_lecturer_of(lectures[0]),
        )
    return CurrentLectureInfo(in_lecture=is_in_lecture, is_lecturer=False)
