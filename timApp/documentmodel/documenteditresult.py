from typing import Optional, List

from timApp.documentmodel.docparagraph import DocParagraph


class DocumentEditResult:
    def __init__(self,
                 added: Optional[List[DocParagraph]]=None,
                 deleted: Optional[List[DocParagraph]]=None,
                 changed: Optional[List[DocParagraph]]=None):
        self.added: List[DocParagraph] = added or []
        self.deleted: List[DocParagraph] = deleted or []
        self.changed: List[DocParagraph] = changed or []

    @property
    def empty(self):
        return not self.added and not self.deleted and not self.changed

    @property
    def pars_added_or_deleted(self):
        return bool(self.added or self.deleted)

    @property
    def new_par_ids(self):
        return [p.get_id() for p in self.added]
