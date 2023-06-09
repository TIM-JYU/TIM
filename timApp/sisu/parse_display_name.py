import re

import attr

from timApp.util.utils import remove_path_special_chars

display_name_re = re.compile(
    r"(?P<coursecode>[A-Z]+\d+) ((?P<period>P\d) )?(?P<dates>(?P<y>\d{4})-(?P<m>\d{2})-(?P<d>\d{2})--\d{4}-\d{2}-\d{2}): (?P<desc>.+)"
)

# These are for converting the Sisu display name into English.
translations = [
    ("opetusryhmien-opettajat", "studysubgroup-teachers"),
    ("opetusryhmien-opiskelijat", "studysubgroup-students"),
    ("kaikki-opiskelijat", "students"),
    ("opiskelijat", "students"),
    ("opettajat", "teachers"),
    # These three entries fix inconsistent pluralization.
    ("teacher", "teachers"),
    ("responsible-teacher", "responsible-teachers"),
    ("administrative-person", "administrative-persons"),
    ("contact-info", "contact-infos"),
]


@attr.s(auto_attribs=True)
class SisuDisplayName:
    coursecode: str
    fulldaterange: str
    year: str
    month: str
    day: str
    desc: str
    period: str | None

    @property
    def group_doc_root(self) -> str:
        return f"groups/{self.year}/{self.coursecode.lower()}/{self.month}"

    @property
    def sisugroups_doc_path(self) -> str:
        return f"{self.group_doc_root}/sisugroups"

    @property
    def coursecode_and_time(self) -> str:
        return f'{self.coursecode.upper()} {self.period + " " if self.period else ""}{self.fulldaterange}'

    @property
    def desc_slug(self) -> str:
        """Returns the group description all-lowercase, spaces replaced with '-' and special characters removed."""
        desc = remove_path_special_chars(self.desc.lower())
        for f, t in translations:
            if desc.endswith(f):
                desc = desc.replace(f, t)
                break
        desc = desc.replace("rooli---", "")
        return desc


def parse_sisu_group_display_name(s: str) -> SisuDisplayName | None:
    m = display_name_re.fullmatch(s)
    if not m:
        return None
    coursecode, period, fulldaterange, year, month, day, desc = (
        m.group("coursecode"),
        m.group("period"),
        m.group("dates"),
        m.group("y"),
        m.group("m"),
        m.group("d"),
        m.group("desc"),
    )
    return SisuDisplayName(
        coursecode=coursecode,
        fulldaterange=fulldaterange,
        year=year,
        month=month,
        day=day,
        desc=desc,
        period=period,
    )
