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
    assert(doc.getVersion() == (0, 0))

    p = doc.addParagraph('testikappale')
    assert(doc.getVersion() == (1, 0))
    assert(len(doc) == 1)
    p1 = DocParagraph.getLatest(p.getId(), files_root=test_files)
    assert(p1.dict() == p.dict())
    assert(p1.getMarkdown() == 'testikappale')

    p2 = doc.addParagraph('kakkoskappale')
    assert(doc.getVersion() == (2, 0))
    assert(len(doc) == 2)
    pp2 = DocParagraph.getLatest(p2.getId(), files_root=test_files)
    assert(pp2.dict() == p2.dict())
    assert(p2.getMarkdown() == 'kakkoskappale')

    assert([par.getMarkdown() for par in doc] == ['testikappale', 'kakkoskappale'])

    p1 = doc.modifyParagraph(p.getId(), 'ykköskappale')
    assert(doc.getVersion() == (2, 1))
    assert(len(doc) == 2)
    assert(p.getMarkdown() == 'testikappale')
    assert(p1.getMarkdown() == 'ykköskappale')

    p2new = doc.insertParagraph('välikappale', p2.getId())
    assert(doc.getVersion() == (3, 0))
    assert(len(doc) == 3)
    assert([par.getMarkdown() for par in doc] == ['ykköskappale', 'välikappale', 'kakkoskappale'])

    doc.deleteParagraph(p2new.getId(), p1.getId())
    assert(doc.getVersion() == (4, 0))
    assert([par.getMarkdown() for par in doc] == ['ykköskappale', 'kakkoskappale'])

    doc.deleteParagraph(p2.getId(), p1.getId())
    assert(doc.getVersion() == (5, 0))
    assert([par.getMarkdown() for par in doc] == ['ykköskappale'])

    doc.deleteParagraph(p1.getId(), None)
    assert(doc.getVersion() == (6, 0))
    assert(len(doc) == 0)

    Document.remove(1, files_root=test_files)
    assert(not os.path.exists(os.path.join(test_files, 'docs', '1')))
