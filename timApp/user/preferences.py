import re
import sre_constants
from typing import Dict, Optional, List

import attr

from timApp.item.item import Item
from timApp.user.settings.theme import Theme
from timApp.user.settings.theme_css import generate_theme, get_default_scss_gen_dir
from timApp.util.utils import cached_property


@attr.s(auto_attribs=True)
class Preferences:
    css_files: Dict[str, bool] = attr.Factory(dict)
    custom_css: str = ''
    use_document_word_list: bool = False
    disable_menu_hover: bool = False
    remember_last_sidebar_menu_tab: bool = False
    remember_last_sidebar_menu_state: bool = False
    word_list: str = ''
    email_exclude: str = ''
    language: Optional[str] = None
    last_answer_fetch: Dict[str, str] = attr.Factory(dict)
    css_combined: str = attr.ib(init=False)
    auto_mark_all_read: bool = False

    @staticmethod
    def from_json(j: Dict) -> 'Preferences':
        j.pop('css_combined', None)
        return Preferences(**j)

    def __attrs_post_init__(self):
        self.css_combined = generate_theme(self.themes, get_default_scss_gen_dir())

    @property
    def themes(self) -> List[Theme]:
        css_file_list = [css for css, v in self.css_files.items() if v]
        return [Theme(f) for f in css_file_list]

    @cached_property
    def excluded_email_paths(self):
        if not isinstance(self.email_exclude, str):
            return []
        try:
            return [re.compile(s) for s in self.email_exclude.strip().splitlines()]
        except sre_constants.error:
            return []

    def is_item_excluded_from_emails(self, d: Item):
        return any(pat.search(d.path) for pat in self.excluded_email_paths)

    def to_json(self):
        return self.__dict__
