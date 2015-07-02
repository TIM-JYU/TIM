# Run from parent directory with command:
# python3 -m unittest dumboclient filemodehelper documentmodel/test_document.py

import os
import unittest
import shutil

import dumboclient
import documentmodel.randutils

from filemodehelper import change_permission_and_retry
from documentmodel.document import Document
from documentmodel.exceptions import DocExistsError
from documentmodel.randutils import random_paragraph

class DocumentTest(unittest.TestCase):
    files_root = 'doctest_files'
    dumbo = dumboclient.launch_dumbo()

    def cleanup(self):
        if os.path.exists(self.files_root):
            shutil.rmtree(self.files_root, onerror=change_permission_and_retry)

    def init_testdoc(self):
        self.cleanup()
        d = Document(doc_id=1, files_root=self.files_root)
        d.create()
        return d
    
    def add_pars(self, d, num_docs):
        pars = [d.add_paragraph(random_paragraph()).get_id() for _ in range(0, num_docs)]
        self.assertEqual((num_docs, 0), d.get_version())
        return pars


    def test_document_create(self):
        self.cleanup()
        d = Document(doc_id=1, files_root=self.files_root)
        self.assertFalse(Document.exists(1, files_root=self.files_root))
        d.create()
        self.assertTrue(Document.exists(1, files_root=self.files_root))
        self.assertEqual(2, Document.get_next_free_id(self.files_root))
        self.assertEqual((0, 0), d.get_version())

        d = Document(doc_id=2, files_root=self.files_root)
        self.assertFalse(Document.exists(2, files_root=self.files_root))
        d.create()
        self.assertTrue(Document.exists(2, files_root=self.files_root))
        self.assertEqual(3, Document.get_next_free_id(self.files_root))
        self.assertEqual((0, 0), d.get_version())

        with self.assertRaises(DocExistsError):
            d.create()


    def test_addparagraph(self):
        d = self.init_testdoc()
       
        # Add first paragraph
        par1 = d.add_paragraph('testing')
        self.assertEqual('testing', par1.get_markdown())
        self.assertTrue(d.has_paragraph(par1.get_id()))
        self.assertEqual((1, 0), d.get_version())

        # Add different next paragraph
        par2 = d.add_paragraph('different')
        self.assertEqual('different', par2.get_markdown())
        self.assertTrue(d.has_paragraph(par2.get_id()))
        self.assertEqual((2, 0), d.get_version())
        self.assertNotEqual(par1.get_id(), par2.get_id())

        # Add next paragraph with same text as the first
        par3 = d.add_paragraph('testing')
        self.assertEqual('testing', par3.get_markdown())
        self.assertTrue(d.has_paragraph(par3.get_id()))
        self.assertEqual((3, 0), d.get_version())
        self.assertNotEqual(par1.get_id(), par2.get_id())

        # Add an empty paragraph
        par3 = d.add_paragraph('')
        self.assertEqual('', par3.get_markdown())
        self.assertTrue(d.has_paragraph(par3.get_id()))
        self.assertEqual((4, 0), d.get_version())
        self.assertNotEqual(par2.get_id(), par3.get_id())
        self.assertNotEqual(par1.get_id(), par3.get_id())

    def test_iterator(self):
        d = self.init_testdoc()

        pars = [d.add_paragraph(random_paragraph()) for _ in range(0, 10)]
        self.assertEqual((10, 0), d.get_version())
        self.assertListEqual([p.get_id() for p in pars], [par.get_id() for par in d])
        self.assertListEqual([p.get_hash() for p in pars], [par.get_hash() for par in d])

    def test_delparagraph(self):
        d = self.init_testdoc()
        pars = self.add_pars(d, 10)

        # Delete first paragraph
        d.delete_paragraph(pars[0])
        self.assertFalse(d.has_paragraph(pars[0]))
        pars.remove(pars[0])
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((11, 0), d.get_version())

        # Delete from the middle
        d.delete_paragraph(pars[2])
        self.assertFalse(d.has_paragraph(pars[2]))
        pars.remove(pars[2])
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((12, 0), d.get_version())

        # Delete last paragraph
        n = len(pars)
        d.delete_paragraph(pars[n-1])
        self.assertFalse(d.has_paragraph(pars[n-1]))
        pars.remove(pars[n-1])
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((13, 0), d.get_version())
        

    def test_insertparagraph(self):
        d = self.init_testdoc()
        pars = self.add_pars(d, 10)

        # Insert as first
        par = d.insert_paragraph('new first', pars[0])
        pars = [par.get_id()] + pars
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((11, 0), d.get_version())

        # Insert in the middle
        par = d.insert_paragraph('middle', pars[4])
        pars = pars[0:4] + [par.get_id()] + pars[4:]
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((12, 0), d.get_version())

        # Insert as last
        par = d.insert_paragraph('last', None)
        pars.append(par.get_id())
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((13, 0), d.get_version())
        
   
    def test_get_html(self):
        d = self.init_testdoc()
        
        par1 = d.add_paragraph('just text')
        self.assertEqual('<p>just text</p>', par1.get_html())

        par1 = d.add_paragraph('# Heading')
        self.assertEqual('<h1 id="heading">Heading</h1>', par1.get_html())


    def test_modify(self):
        d = self.init_testdoc()
        pars = [d.add_paragraph(random_paragraph()) for _ in range(0, 10)]
        self.assertEqual((10, 0), d.get_version())
        
        par2_id = pars[2].get_id()
        par2_hash = pars[2].get_hash()
        par2_mod = d.modify_paragraph(par2_id, 'new text')
        
        self.assertEqual(par2_id, par2_mod.get_id())
        self.assertEqual('new text', par2_mod.get_markdown())
        self.assertNotEqual(par2_hash, par2_mod.get_hash())
        self.assertEqual((10, 1), d.get_version())

        for i in range(0, 10):
            par2_id = pars[i].get_id()
            par2_hash = pars[i].get_hash()
            new_text = random_paragraph()
            par2_mod = d.modify_paragraph(par2_id, new_text)
            self.assertEqual(par2_id, par2_mod.get_id())
            self.assertEqual(new_text, par2_mod.get_markdown())
            self.assertNotEqual(par2_hash, par2_mod.get_hash())
            self.assertEqual((10, i + 2), d.get_version())


    def test_document_remove(self):
        self.cleanup()

        self.assertFalse(Document.exists(doc_id=1, files_root=self.files_root))
        self.assertFalse(Document.exists(doc_id=2, files_root=self.files_root))
        self.assertFalse(Document.exists(doc_id=3, files_root=self.files_root))
        self.assertFalse(Document.exists(doc_id=4, files_root=self.files_root))
        self.assertFalse(Document.exists(doc_id=5, files_root=self.files_root))

        for i in range(1,6):
            if i != 3:
                d = Document(doc_id=i, files_root=self.files_root)
                d.create()

        self.assertEqual(6, Document.get_next_free_id(files_root=self.files_root))

        with self.assertRaises(DocExistsError):
            Document.remove(doc_id=3, files_root=self.files_root)

        Document.remove(doc_id=2, files_root=self.files_root)
        self.assertFalse(Document.exists(doc_id=2, files_root=self.files_root))
        self.assertEqual(6, Document.get_next_free_id(files_root=self.files_root))

        Document.remove(doc_id=5, files_root=self.files_root)
        self.assertFalse(Document.exists(doc_id=5, files_root=self.files_root))
        self.assertEqual(5, Document.get_next_free_id(files_root=self.files_root))

        Document.remove(doc_id=1, files_root=self.files_root)
        self.assertFalse(Document.exists(doc_id=1, files_root=self.files_root))
        self.assertEqual(5, Document.get_next_free_id(files_root=self.files_root))

        Document.remove(doc_id=4, files_root=self.files_root)
        self.assertFalse(Document.exists(doc_id=4, files_root=self.files_root))
        self.assertEqual(1, Document.get_next_free_id(files_root=self.files_root))

    @classmethod
    def tearDownClass(cls):
        DocumentTest.dumbo.kill()


if __name__ == '__main__':
    unittest.main()
