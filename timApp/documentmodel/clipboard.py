import os
import shutil

from documentmodel.document import Document
from documentmodel.documentwriter import DocumentParser, DocumentWriter
from typing import Dict, List, Optional, TypeVar


ClipboardType = TypeVar('Clipboard')


class Clipboard:
    def __init__(self, files_root: str):
        self.files_root = files_root

    def get_path(self):
        return os.path.join(self.files_root, 'clipboard')

    def get(self, user_id: int):
        return Clipboard.UserClipboard(self, user_id)

    def clear_all(self):
        path = self.get_path()
        if os.path.exists(path):
            shutil.rmtree(path)

    class UserClipboard:
        def __init__(self, parent: ClipboardType, user_id: int):
            self.user_id = user_id
            self.path = os.path.join(parent.get_path(), str(self.user_id))

        def get_clipfilename(self) -> str:
            return os.path.join(self.path, 'content')

        def clear(self):
            clipfilename = self.get_clipfilename()
            if os.path.isfile(clipfilename):
                os.remove(clipfilename)

        def read(self) -> Optional[List[Dict]]:
            clipfilename = self.get_clipfilename()
            if not os.path.isfile(clipfilename):
                return None
            with open(clipfilename, 'rt', encoding='utf-8') as clipfile:
                content = clipfile.read()
            return DocumentParser(content).validate_structure(is_whole_document=False).get_blocks()

        def write(self, pars: List[Dict]):
            os.makedirs(self.path, exist_ok=True)
            text = DocumentWriter(pars).get_text()
            with open(self.get_clipfilename(), 'wt', encoding='utf-8') as clipfile:
                clipfile.write(text)

        def copy_pars(self, doc: Document, par_start: str, par_end: str):
            copying = False
            pars = []

            # todo: make the iterator accept ranges
            i = doc.__iter__()
            try:
                while True:
                    par = next(i)
                    if not copying and par.get_id() == par_start:
                        copying = True
                    if copying:
                        pars.append(par.dict())
                        if par.get_id() == par_end:
                            raise StopIteration
            except StopIteration:
                pass
            finally:
                i.close()

            self.write(pars)

        def paste_before(self, doc: Document, par_id: Optional[str]):
            pars = self.read()
            if pars is None:
                return
            par_before = par_id
            for par in reversed(pars):
                # We need to reverse the sequence because we're inserting before, not after
                new_par = doc.insert_paragraph(par['md'], par_before, attrs=par.get('attrs'), properties=par.get('properties'))
                par_before = new_par.get_id()
