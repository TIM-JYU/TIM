from typing import Generator, Tuple, Optional

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.folder import Folder


def enum_docs(folder: Optional[Folder] = None) -> Generator[DocInfo, None, None]:
    visited_docs = set()
    if not folder:
        folder = Folder.get_root()
    for d in folder.get_all_documents(include_subdirs=True):
        for t in d.translations:
            if t.id in visited_docs:
                continue
            visited_docs.add(t.id)
            yield t


def enum_pars(folder: Optional[Folder] = None) -> Generator[Tuple[DocInfo, DocParagraph], None, None]:
    for d in enum_docs(folder):
        for p in d.document.get_paragraphs():
            yield d, p
