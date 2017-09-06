from typing import Generator, Tuple

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.docentry import DocEntry


def enum_docs() -> Generator[DocInfo, None, None]:
    visited_docs = set()
    for d in DocEntry.get_all():
        for t in d.translations:
            if t.id in visited_docs:
                continue
            visited_docs.add(t.id)
            yield t


def enum_pars() -> Generator[Tuple[DocInfo, DocParagraph], None, None]:
    for d in enum_docs():
        for p in d.document.get_paragraphs():
            yield d, p
