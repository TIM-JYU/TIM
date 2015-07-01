import os
import unittest
import shutil
from documentmodel.document import Document
import dumboclient
from filemodehelper import change_permission_and_retry


class DocumentTest(unittest.TestCase):
    files_root = 'doctest_files'
    dumbo = dumboclient.launch_dumbo()

    @classmethod
    def setUpClass(cls):
        if os.path.exists(DocumentTest.files_root):
            shutil.rmtree(DocumentTest.files_root, onerror=change_permission_and_retry)

    def test_document(self):
        d = Document(doc_id=1, files_root=DocumentTest.files_root)
        self.assertFalse(Document.exists(1, files_root=DocumentTest.files_root))
        d.create()
        self.assertTrue(Document.exists(1, files_root=DocumentTest.files_root))
        self.assertEqual(2, Document.get_next_free_id(self.files_root))
        self.assertEqual((0, 0), d.get_version())
        par = d.add_paragraph('testing')
        self.assertEqual('testing', par.get_markdown())
        self.assertTrue(d.has_paragraph(par.get_id()))
        self.assertEqual((1, 0), d.get_version())
        d.delete_paragraph(par.get_id())
        self.assertFalse(d.has_paragraph(par.get_id()))
        self.assertEqual((2, 0), d.get_version())
        par = d.add_paragraph('first')
        self.assertEqual((3, 0), d.get_version())
        par2 = d.add_paragraph('second')
        self.assertEqual((4, 0), d.get_version())
        self.assertListEqual([], d.get_index())
        self.assertListEqual(['first', 'second'], [p.get_markdown() for p in d])
        self.assertListEqual(['<p>first</p>', '<p>second</p>'], [p.get_html() for p in d])
        par3_new = d.modify_paragraph(par2.get_id(), 'third')
        self.assertEqual((4, 1), d.get_version())
        self.assertEqual(par2.get_id(), par3_new.get_id())
        self.assertNotEqual(par2.get_hash(), par3_new.get_hash())
        par2_new = d.insert_paragraph('new second', par3_new.get_id())
        self.assertEqual((5, 0), d.get_version())
        self.assertListEqual(['first', 'new second', 'third'], [p.get_markdown() for p in d])
        Document.remove(doc_id=1, files_root=DocumentTest.files_root)
        self.assertFalse(Document.exists(1, files_root=DocumentTest.files_root))

    @classmethod
    def tearDownClass(cls):
        DocumentTest.dumbo.kill()


if __name__ == '__main__':
    unittest.main()
