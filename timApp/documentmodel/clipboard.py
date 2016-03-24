import os

from documentmodel.document import Document
from documentmodel.documentwriter import DocumentParser, DocumentWriter
from documentmodel.docparagraph import DocParagraph
from typing import Dict, List, Optional


class Clipboard:
    def __init__(self, files_root: str):
        self.files_root = files_root

    def get(self, user_id: int):
        return Clipboard.UserClipboard(user_id, self.files_root)

    class UserClipboard:
        def __init__(self, user_id: int, FILES_ROOT: str):
            self.user_id = user_id
            self.path = os.path.join(FILES_ROOT, 'clipboard', str(self.user_id))

        def get_clipfilename(self) -> str:
            return os.path.join(self.path, 'content')

        def read(self) -> Optional[List[Dict]]:
            clipfilename = self.get_clipfilename()
            if not os.path.isfile(clipfilename):
                return None
            with open(clipfilename, 'r') as clipfile:
                return DocumentParser(clipfile.read()).validate_structure(is_whole_document=False).get_blocks()

        def write(self, pars: List[Dict]):
            os.makedirs(self.path, exist_ok=True)
            text = DocumentWriter(pars).get_text()
            with open(self.get_clipfilename(), 'w') as clipfile:
                clipfile.write(text)

        def copy_pars(self, doc: Document, par_start: str, par_end: str):
            pass

        def paste_before(self, doc: Document, par_id: str):
            pass

