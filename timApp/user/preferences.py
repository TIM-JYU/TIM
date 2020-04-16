import os
import re
import sre_constants
from pathlib import Path
from typing import Dict, Optional

import attr
from flask import current_app

from timApp.item.item import Item
from timApp.user.settings.theme import Theme
from timApp.user.settings.theme_css import generate_theme_scss, ThemeNotFoundException, get_combined_css_filename
from timApp.util.utils import cached_property


static_folder = Path('static')


@attr.s(auto_attribs=True)
class Preferences:
    css_files: Dict[str, bool] = attr.Factory(dict)
    custom_css: str = ''
    use_document_word_list: bool = False
    disable_menu_hover: bool = False
    word_list: str = ''
    email_exclude: str = ''
    language: Optional[str] = None
    last_answer_fetch: Dict[str, str] = attr.Factory(dict)
    css_combined: str = attr.ib(init=False)

    @staticmethod
    def from_json(j):
        j.pop('css_combined', None)
        return Preferences(**j)

    def __attrs_post_init__(self):
        css_file_list = [css for css, v in self.css_files.items() if v]
        css_file_list.sort()
        theme_list = [Theme(f) for f in css_file_list]
        try:
            generate_theme_scss(theme_list, static_folder / current_app.config['SASS_GEN_PATH'])
        except ThemeNotFoundException:
            theme_list = []
            generate_theme_scss(theme_list, static_folder / current_app.config['SASS_GEN_PATH'])
        self.css_combined = get_combined_css_filename(theme_list)

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
