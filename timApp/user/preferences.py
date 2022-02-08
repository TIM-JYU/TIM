import re
import sre_constants
from functools import cached_property
from re import Pattern
from typing import Optional

import attr

from timApp.document.docentry import DocEntry
from timApp.item.item import Item

BookmarkEntry = dict[str, str]
BookmarkEntryList = list[BookmarkEntry]
BookmarkFolder = dict[str, BookmarkEntryList]
BookmarkCollection = list[BookmarkFolder]


@attr.s(auto_attribs=True)
class Preferences:
    custom_css: str = ""
    use_document_word_list: bool = False
    disable_menu_hover: bool = False
    remember_last_sidebar_menu_tab: bool = False
    remember_last_sidebar_menu_state: bool = False
    word_list: str = ""
    email_exclude: str = ""
    language: str | None = None
    style_doc_ids: list[int] = attr.Factory(list)
    last_answer_fetch: dict[str, str] = attr.Factory(dict)
    auto_mark_all_read: bool = False
    bookmarks: BookmarkCollection | None = None
    max_uncollapsed_toc_items: int | None = None

    @staticmethod
    def from_json(j: dict) -> "Preferences":
        j.pop("style_path", None)
        return Preferences(**j)

    def theme_docs(self) -> list[DocEntry]:
        if not self.style_doc_ids:
            return []
        ordering = {d: i for i, d in enumerate(self.style_doc_ids)}
        return sorted(
            DocEntry.query.filter(DocEntry.id.in_(self.style_doc_ids)).all(),
            key=lambda d: ordering[d.id],
        )

    @cached_property
    def style_path(self) -> str:
        from timApp.user.settings.styles import generate_style

        style_path, style_hash = generate_style(self.theme_docs())
        return f"{style_path}?{style_hash}"

    @cached_property
    def excluded_email_paths(self) -> list[Pattern[str]]:
        if not isinstance(self.email_exclude, str):
            return []
        try:
            return [re.compile(s) for s in self.email_exclude.strip().splitlines()]
        except sre_constants.error:
            return []

    def is_item_excluded_from_emails(self, d: Item) -> bool:
        return any(pat.search(d.path) for pat in self.excluded_email_paths)

    def to_json(self, with_style: bool = False) -> dict:
        result = self.__dict__
        if with_style:
            result |= {"style_path": self.style_path}
        return result
