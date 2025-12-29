import enum
from typing import Any


class AccessType(enum.Enum):
    view = 1
    edit = 2
    teacher = 3
    manage = 4
    see_answers = 5
    owner = 6
    copy = 7

    @classmethod
    def from_str(cls, s: Any) -> "AccessType":
        """
        Map a string (or numeric string) to an AccessType.
        Accepts case-insensitive names, hyphen/underscore variants, short aliases and numeric values.
        Raises ValueError for unknown input.
        """
        if s is None:
            raise ValueError("AccessType value is required")

        # Accept enum member directly
        if isinstance(s, cls):
            return s

        # Accept ints or numeric strings
        if isinstance(s, int):
            return cls(s)
        s_str = str(s).strip().lower()

        # Try numeric conversion
        if s_str.isdigit():
            try:
                return cls(int(s_str))
            except ValueError:
                pass

        aliases = {
            "v": cls.view,
            "view": cls.view,
            "e": cls.edit,
            "edit": cls.edit,
            "t": cls.teacher,
            "teacher": cls.teacher,
            "m": cls.manage,
            "manage": cls.manage,
            "see_answers": cls.see_answers,
            "see-answers": cls.see_answers,
            "see answers": cls.see_answers,
            "seeanswers": cls.see_answers,
            "s": cls.see_answers,
            "o": cls.owner,
            "owner": cls.owner,
            "c": cls.copy,
            "copy": cls.copy,
        }

        try:
            return aliases[s_str]
        except KeyError:
            raise ValueError(f"Unknown AccessType: {s}")

    def __str__(self) -> str:
        return self.name
