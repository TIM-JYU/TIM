from typing import Optional

from timApp.document.docparagraph import DocParagraph


class DocumentEditResult:
    def __init__(
        self,
        added: list[DocParagraph] | None = None,
        deleted: list[DocParagraph] | None = None,
        changed: list[DocParagraph] | None = None,
        changes: Optional[list[str]] = None,
    ):
        self.added: list[DocParagraph] = added or []
        self.deleted: list[DocParagraph] = deleted or []
        self.changed: list[DocParagraph] = changed or []
        self.changes: Optional[list[str]] = changes or None

    @property
    def empty(self):
        return not self.added and not self.deleted and not self.changed

    @property
    def pars_added_or_deleted(self):
        return bool(self.added or self.deleted)

    @property
    def new_par_ids(self):
        return [p.get_id() for p in self.added]
