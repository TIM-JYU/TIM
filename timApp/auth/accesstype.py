import enum


class AccessType(enum.Enum):
    view = 1
    edit = 2
    teacher = 3
    manage = 4
    see_answers = 5
    owner = 6
    copy = 7
