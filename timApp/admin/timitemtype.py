from typing import Any

import click

from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.item.item import Item


class TimItemType(click.ParamType):
    """TIM item."""
    name = "timitem"

    def convert(self, value: Any, param: Any, ctx: Any) -> Item:
        d = DocEntry.find_by_path(value)
        if d:
            return d
        f = Folder.find_by_path(value)
        if f:
            return f
        self.fail(f'Item {value} not found.')


class TimDocumentType(click.ParamType):
    """TIM document."""
    name = "timdocument"

    def convert(self, value: Any, param: Any, ctx: Any) -> Item:
        d = DocEntry.find_by_path(value)
        if d:
            return d
        self.fail(f'Document {value} not found.')
