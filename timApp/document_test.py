import os
import shutil

from documentmodel.document import Document
from documentmodel.docparagraph import DocParagraph

if __name__ == "__main__":
    test_files = 'test_files'
    if os.path.exists(test_files):
        shutil.rmtree(test_files)

    doc = Document(1, files_root=test_files)
    assert(len(doc) == 0)
    assert(doc.get_version() == (0, 0))

    p = doc.add_paragraph('testikappale')
    assert(doc.get_version() == (1, 0))
    assert(len(doc) == 1)
    p1 = DocParagraph.get_latest(p.get_id(), files_root=test_files)
    assert(p1.dict() == p.dict())
    assert(p1.get_markdown() == 'testikappale')

    p2 = doc.add_paragraph('kakkoskappale')
    assert(doc.get_version() == (2, 0))
    assert(len(doc) == 2)
    pp2 = DocParagraph.get_latest(p2.get_id(), files_root=test_files)
    assert(pp2.dict() == p2.dict())
    assert(p2.get_markdown() == 'kakkoskappale')

    assert([par.get_markdown() for par in doc] == ['testikappale', 'kakkoskappale'])

    p1 = doc.modify_paragraph(p.get_id(), 'ykköskappale')
    assert(doc.get_version() == (2, 1))
    assert(len(doc) == 2)
    assert(p.get_markdown() == 'testikappale')
    assert(p1.get_markdown() == 'ykköskappale')

    p2new = doc.insert_paragraph('välikappale', p2.get_id())
    assert(doc.get_version() == (3, 0))
    assert(len(doc) == 3)
    assert([par.get_markdown() for par in doc] == ['ykköskappale', 'välikappale', 'kakkoskappale'])

    doc.delete_paragraph(p2new.get_id(), p1.get_id())
    assert(doc.get_version() == (4, 0))
    assert([par.get_markdown() for par in doc] == ['ykköskappale', 'kakkoskappale'])

    doc.delete_paragraph(p2.get_id(), p1.get_id())
    assert(doc.get_version() == (5, 0))
    assert([par.get_markdown() for par in doc] == ['ykköskappale'])

    doc.delete_paragraph(p1.get_id(), None)
    assert(doc.get_version() == (6, 0))
    assert(len(doc) == 0)

    Document.remove(1, files_root=test_files)
    assert(not os.path.exists(os.path.join(test_files, 'docs', '1')))
