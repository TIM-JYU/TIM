import os
import unittest
import shutil
from documentmodel.document import Document
from filemodehelper import change_permission_and_retry


class DocumentTest(unittest.TestCase):
    files_root = 'doctest_files'

    @classmethod
    def setUpClass(cls):
        if os.path.exists(DocumentTest.files_root):
            shutil.rmtree(DocumentTest.files_root, onerror=change_permission_and_retry)

    def test_document(self):
        d = Document(doc_id=1, files_root=DocumentTest.files_root)
        self.assertTrue(Document.exists(1, files_root=DocumentTest.files_root))
        self.assertEqual(2, Document.getNextFreeId(self.files_root))
        par = d.addParagraph('testing')
        self.assertEqual('testing', par.getMarkdown())
        self.assertTrue(d.hasParagraph(par.getId()))
        d.deleteParagraph(par.getId())
        self.assertFalse(d.hasParagraph(par.getId()))
        par = d.addParagraph('first')
        par2 = d.addParagraph('second')
        self.assertListEqual([], d.get_index())
        self.assertListEqual(['first', 'second'], [p.getMarkdown() for p in d])
        par3_new = d.modifyParagraph(par2.getId(), 'third')
        self.assertEqual(par2.getId(), par3_new.getId())
        self.assertNotEqual(par2.getHash(), par3_new.getHash())
        par2_new = d.insertParagraph('new second', par3_new.getId())
        self.assertListEqual(['first', 'new second', 'third'], [p.getMarkdown() for p in d])

if __name__ == '__main__':
    unittest.main()
