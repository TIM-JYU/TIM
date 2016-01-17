"""Unit tests for Document class.

Run from parent directory with command:
python3 -m unittest dumboclient filemodehelper documentmodel/test_document.py
"""

import random
import unittest

from documentmodel.document import Document
from documentmodel.documentparser import DocumentParser
from documentmodel.documentwriter import DocumentWriter
from documentmodel.exceptions import DocExistsError
from documentmodel.randutils import random_paragraph
from timdbtest import TimDbTest


class DocumentTest(TimDbTest):
    def init_testdoc(self, doc_id=None):
        d = Document(doc_id=doc_id)
        self.assertFalse(d.exists())
        db = self.get_db()
        self.get_db().documents.create(str(d.doc_id), 0, d.doc_id)
        db.close()
        return d
    
    def add_pars(self, d, num_docs):
        pars = [d.add_paragraph(random_paragraph()).get_id() for _ in range(0, num_docs)]
        self.assertEqual((num_docs, 0), d.get_version())
        return pars

    def test_document_create(self):
        d = self.init_testdoc()
        self.assertTrue(d.exists())
        self.assertEqual(d.doc_id + 1, Document.get_next_free_id())
        self.assertEqual((0, 0), d.get_version())
        self.assertListEqual([], d.get_changelog())

        d = self.init_testdoc()
        self.assertTrue(d.exists())
        self.assertEqual(d.doc_id + 1, Document.get_next_free_id())
        self.assertEqual((0, 0), d.get_version())
        self.assertListEqual([], d.get_changelog())

        with self.assertRaises(DocExistsError):
            d.create()

    def test_addparagraph(self):
        d = self.init_testdoc()
       
        # Add first paragraph
        par1 = d.add_paragraph('testing')
        self.assertEqual('testing', par1.get_markdown())
        self.assertTrue(d.has_paragraph(par1.get_id()))
        self.assertFalse(d.has_paragraph(par1.get_id()[:-1]))
        self.assertEqual((1, 0), d.get_version())
        self.assertEqual(1, len(d.get_changelog()))

        # Add different next paragraph
        par2 = d.add_paragraph('different')
        self.assertEqual('different', par2.get_markdown())
        self.assertTrue(d.has_paragraph(par2.get_id()))
        self.assertEqual((2, 0), d.get_version())
        self.assertEqual(2, len(d.get_changelog()))
        self.assertNotEqual(par1.get_id(), par2.get_id())

        # Add next paragraph with same text as the first
        par3 = d.add_paragraph('testing')
        self.assertEqual('testing', par3.get_markdown())
        self.assertTrue(d.has_paragraph(par3.get_id()))
        self.assertEqual((3, 0), d.get_version())
        self.assertEqual(3, len(d.get_changelog()))
        self.assertNotEqual(par1.get_id(), par2.get_id())

        # Add an empty paragraph
        par3 = d.add_paragraph('')
        self.assertEqual('', par3.get_markdown())
        self.assertTrue(d.has_paragraph(par3.get_id()))
        self.assertEqual((4, 0), d.get_version())
        self.assertEqual(4, len(d.get_changelog()))
        self.assertNotEqual(par2.get_id(), par3.get_id())
        self.assertNotEqual(par1.get_id(), par3.get_id())

    def test_iterator(self):
        d = self.init_testdoc()

        pars = [d.add_paragraph(random_paragraph()) for _ in range(0, 10)]
        self.assertEqual((10, 0), d.get_version())
        self.assertEqual(10, len(d.get_changelog()))
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
        self.assertEqual(11, len(d.get_changelog()))

        # Delete from the middle
        d.delete_paragraph(pars[2])
        self.assertFalse(d.has_paragraph(pars[2]))
        pars.remove(pars[2])
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((12, 0), d.get_version())
        self.assertEqual(12, len(d.get_changelog()))

        # Delete last paragraph
        n = len(pars)
        d.delete_paragraph(pars[n-1])
        self.assertFalse(d.has_paragraph(pars[n-1]))
        pars.remove(pars[n-1])
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((13, 0), d.get_version())
        self.assertEqual(13, len(d.get_changelog()))

    def test_insertparagraph(self):
        d = self.init_testdoc()
        pars = self.add_pars(d, 10)

        # Insert as first
        par = d.insert_paragraph('new first', pars[0])
        pars = [par.get_id()] + pars
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((11, 0), d.get_version())
        self.assertEqual(11, len(d.get_changelog()))

        # Insert in the middle
        par = d.insert_paragraph('middle', pars[4])
        pars = pars[0:4] + [par.get_id()] + pars[4:]
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((12, 0), d.get_version())
        self.assertEqual(12, len(d.get_changelog()))

        # Insert as last
        par = d.insert_paragraph('last', None)
        pars.append(par.get_id())
        self.assertListEqual(pars, [par.get_id() for par in d])
        self.assertEqual((13, 0), d.get_version())
        self.assertEqual(13, len(d.get_changelog()))

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
        old_md = pars[2].get_markdown()
        new_text = 'new_text'
        par2_mod = d.modify_paragraph(par2_id, new_text)

        self.assertEqual(par2_id, par2_mod.get_id())
        self.assertEqual(new_text, d.get_paragraph(par2_id).get_markdown())
        self.assertEqual(new_text, par2_mod.get_markdown())
        self.assertNotEqual(par2_hash, par2_mod.get_hash())
        self.assertEqual((10, 1), d.get_version())
        self.assertEqual(11, len(d.get_changelog()))

        par2_mod = d.modify_paragraph(par2_id, old_md)
        self.assertEqual(old_md, par2_mod.get_markdown())
        self.assertEqual(old_md, d.get_paragraph(par2_id).get_markdown())

        for i in range(0, 10):
            par2_id = pars[i].get_id()
            par2_hash = pars[i].get_hash()
            new_text = random_paragraph()
            par2_mod = d.modify_paragraph(par2_id, new_text)
            self.assertEqual(par2_id, par2_mod.get_id())
            self.assertEqual(new_text, par2_mod.get_markdown())
            self.assertNotEqual(par2_hash, par2_mod.get_hash())
            self.assertEqual((10, i + 3), d.get_version())
            self.assertEqual(13 + i, len(d.get_changelog()))

    def test_document_remove(self):
        free = Document.get_next_free_id()
        db = self.get_db()
        for i in range(free, free + 5):
            if i != free + 2:
                self.init_testdoc(i)

        self.assertEqual(free + 5, Document.get_next_free_id())

        with self.assertRaises(DocExistsError):
            Document.remove(doc_id=free + 2)

        db.documents.delete(free + 1)
        self.assertFalse(Document.doc_exists(doc_id=free + 1))
        self.assertEqual(free + 5, Document.get_next_free_id())

        db.documents.delete(free + 4)
        self.assertFalse(Document.doc_exists(doc_id=free + 4))
        self.assertEqual(free + 4, Document.get_next_free_id())

        db.documents.delete(free)
        self.assertFalse(Document.doc_exists(doc_id=free))
        self.assertEqual(free + 4, Document.get_next_free_id())

        db.documents.delete(free + 3)
        self.assertFalse(Document.doc_exists(doc_id=free + 3))
        self.assertEqual(free, Document.get_next_free_id())
        db.close()

    def test_update(self):
        self.maxDiff = None
        random.seed(0)
        for i in range(1, 5):
            d = self.init_testdoc()
            for _ in range(0, i):
                d.add_paragraph(random_paragraph())
            fulltext = d.export_markdown()
            d.update(fulltext, fulltext)
            self.assertEqual(fulltext, d.export_markdown())
            dp = DocumentParser(fulltext)
            blocks = dp.get_blocks()
            random.shuffle(blocks)
            blocks[0]['md'] = 'modified'
            new_text = DocumentWriter(blocks).get_text()
            d.update(new_text, fulltext)
            blocks = DocumentParser(new_text).add_missing_attributes().get_blocks()
            self.assertListEqual(blocks, DocumentParser(d.export_markdown(export_hashes=True)).get_blocks())

    def test_update_section(self):
        self.maxDiff = None
        random.seed(0)
        for i in range(6, 10):
            d = self.init_testdoc()
            for _ in range(0, i):
                d.add_paragraph(random_paragraph())
            ids = [par.get_id() for par in d]
            new_pars = DocumentParser('#-\none\n\n#-\ntwo\n\n#-\nthree').add_missing_attributes().get_blocks()
            start_repl_index = 1
            end_repl_index = 4
            repl_length = len(new_pars)
            length_diff = repl_length - (end_repl_index - start_repl_index + 1)
            section_text = DocumentWriter(new_pars).get_text()
            d.update_section(section_text, ids[start_repl_index], ids[end_repl_index])
            new_ids = [par.get_id() for par in d]
            self.assertListEqual([par['id'] for par in new_pars],
                                 new_ids[start_repl_index:start_repl_index + repl_length])
            self.assertEqual(length_diff, len(new_ids) - len(ids))

    def test_macros(self):
        d = self.init_testdoc()
        settings_par = d.add_paragraph('```\n'
                                       'macro_delimiter: "%%"\n'
                                       'macros:\n'
                                       ' testmacro: testvalue\n'
                                       ' year: "2015"\n'
                                       '```', attrs={'settings': ''})
        macro_par = d.add_paragraph('this is %%testmacro%% and year is %%year%%')
        macro_par = d.get_paragraph(macro_par.get_id())  # Put the paragraph in cache
        self.assertDictEqual({'macros': {'testmacro': 'testvalue', 'year': '2015'},
                              'macro_delimiter': '%%'}, d.get_settings().get_dict())

        self.assertEqual('<p>this is testvalue and year is 2015</p>', macro_par.get_html())
        d = Document(d.doc_id)  # Make a new instance of the document to test cache invalidation
        d.modify_paragraph(settings_par.get_id(),
                           '```\n'
                           'macro_delimiter: "%%"\n'
                           'macros:\n'
                           ' testmacro: anothervalue\n'
                           ' year: "2016"\n'
                           '```',
                           new_attrs={'settings': ''})

        macro_par = d.get_paragraph(macro_par.get_id())
        self.assertEqual('<p>this is anothervalue and year is 2016</p>', macro_par.get_html())

    def test_import(self):
        timdb = self.get_db()
        from timdb.users import ANONYMOUS_GROUPNAME
        anon_group = timdb.users.getUserGroupByName(ANONYMOUS_GROUPNAME)
        timdb.documents.import_document_from_file('example_docs/mmcq_example.md',
                                                  'Multiple choice plugin example',
                                                  anon_group)
        timdb.close()

if __name__ == '__main__':
    unittest.main()
