from typing import Optional, List

from timApp.document.docparagraph import DocParagraph


class DocumentEditResult:
    def __init__(self,
                 added: Optional[list[DocParagraph]]=None,
                 deleted: Optional[list[DocParagraph]]=None,
                 changed: Optional[list[DocParagraph]]=None):
        self.added: list[DocParagraph] = added or []
        self.deleted: list[DocParagraph] = deleted or []
        self.changed: list[DocParagraph] = changed or []

    @property
    def empty(self):
        return not self.added and not self.deleted and not self.changed

    @property
    def pars_added_or_deleted(self):
        return bool(self.added or self.deleted)

    @property
    def new_par_ids(self):
        return [p.get_id() for p in self.added]
