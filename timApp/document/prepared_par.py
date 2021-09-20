from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Optional, List, TYPE_CHECKING

from timApp.document.par_basic_data import ParBasicData

if TYPE_CHECKING:
    from timApp.document.areainfo import AreaBoundary
    from timApp.document.changelog import AuthorInfo
    from timApp.note.notes import UserNoteAndUser
    from timApp.readmark.readmarkcollection import ReadMarkCollection

NEEDS_ANGULAR_ATTRS = ('plugin', 'defaultplugin', 'gamification')


@dataclass
class PreparedPar:
    """Represents a "prepared" paragraph that is ready to be rendered (e.g. to HTML)."""
    data: ParBasicData
    target: Optional[ParBasicData]
    output: str
    html_class: str
    from_preamble: Optional[str]
    authorinfo: Optional[AuthorInfo] = None
    status: Optional[ReadMarkCollection] = None
    notes: Optional[List[UserNoteAndUser]] = None
    areainfo: Optional[AreaBoundary] = None

    @property
    def target_data(self) -> ParBasicData:
        return self.target or self.data

    @property
    def id(self) -> str:
        return self.data.id

    @property
    def doc_id(self) -> int:
        return self.data.doc_id

    @property
    def hash(self) -> str:
        return self.data.hash

    @property
    def attrs(self) -> Dict[str, str]:
        return self.data.attrs

    @property
    def md(self) -> str:
        return self.data.md

    @property
    def attrs_str(self) -> str:
        return self.data.attrs_str

    @property
    def is_setting(self) -> bool:
        return self.data.attrs.get('settings') is not None

    @property
    def needs_angular(self) -> bool:
        attrs = self.target_data.attrs
        return any(attrs.get(x) for x in NEEDS_ANGULAR_ATTRS)

    @property
    def class_str(self) -> str:
        return ' '.join(self.attrs.get('classes', []))
