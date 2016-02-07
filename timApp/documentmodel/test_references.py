"""Unit tests for testing paragraph referencing.

Run from parent directory with command:
python3 -m unittest dumboclient filemodehelper documentmodel/test_references.py
"""

import random
import unittest

from documentmodel.document import Document
from documentmodel.docparagraph import DocParagraph
from documentmodel.documentparser import DocumentParser
from documentmodel.documentwriter import DocumentWriter
from documentmodel.exceptions import DocExistsError
from documentmodel.randutils import random_paragraph
from timdbtest import TimDbTest
from timdb.timdbbase import TimDbException


class RefTest(TimDbTest):
    def doc_create(self, db, doc_name, doc_group):
        doc_id = db.documents.get_document_id(doc_name)
        if doc_id is not None:
            db.documents.delete(doc_id)
        return db.documents.create(doc_name, owner_group_id=doc_group)

    def dict_merge(self, a, b):
        c = a.copy()
        c.update(b)
        return c

    def dict_issubset(self, a, b):
        return set(a.items()).issubset(set(b.items()))

    def assert_dict_issubset(self, a, b):
        self.assertTrue(self.dict_issubset(a, b), "{} is not a subset of {}".format(a, b))


    def init_testdb(self):
        db = self.get_db()
        self.src_doc = self.doc_create(db, "original", 1)
        self.ref_doc = self.doc_create(db, "referencing", 2)

        self.src_par = self.src_doc.add_paragraph("testpar", attrs={"a": "1", "b": "2"},
                                                  properties={"p_a": "a", "p_b": "b"})
        self.assertEqual(1, len(self.src_doc))
        self.assertEqual(self.src_par.get_id(), self.src_doc.get_paragraphs()[0].get_id())
        return db

    def test_simpleref(self):
        db = self.init_testdb()

        ref_par = self.ref_doc.add_ref_paragraph(self.src_par)
        self.assertEqual(1, len(self.ref_doc))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual('', ref_par.get_markdown())

        rendered_pars = ref_par.get_referenced_pars()
        self.assertEqual(1, len(rendered_pars))
        self.assertEqual(self.src_par.get_id(), rendered_pars[0].get_id())
        self.assertEqual(self.src_par.get_markdown(), rendered_pars[0].get_markdown())
        self.assertEqual(self.src_par.get_html(), rendered_pars[0].get_html())
        self.assertEqual(self.src_par.get_attrs(), rendered_pars[0].get_attrs())
        self.assertEqual(self.src_par.get_properties(), rendered_pars[0].get_properties())

        db.close()

    def test_translation(self):
        db = self.init_testdb()

        ref_attrs = {'foo': 'fffoooo', 'bar': 'baaaa'}
        ref_par = self.ref_doc.add_ref_paragraph(self.src_par, "translation", attrs=ref_attrs)
        self.assertEqual(1, len(self.ref_doc))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual("translation", ref_par.get_markdown())

        rendered_pars = ref_par.get_referenced_pars()
        self.assertEqual(1, len(rendered_pars))
        self.assertEqual(self.src_par.get_id(), rendered_pars[0].get_id())
        self.assertEqual(ref_par.get_markdown(), rendered_pars[0].get_markdown())
        self.assertEqual(ref_par.get_html(), rendered_pars[0].get_html())
        self.assertEqual(self.dict_merge(self.src_par.get_attrs(), ref_attrs), rendered_pars[0].get_attrs())
        self.assertEqual(self.dict_merge(self.src_par.get_properties(), ref_par.get_properties()),
                         rendered_pars[0].get_properties())

        db.close()

    def test_circular(self):
        db = self.init_testdb()

        ref_par = self.ref_doc.add_ref_paragraph(self.src_par)
        self.assertEqual(1, len(self.ref_doc))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual('', ref_par.get_markdown())

        self.src_par.set_attr('rd', str(self.ref_doc.doc_id))
        self.src_par.set_attr('rp', ref_par.get_id())
        self.src_doc.modify_paragraph_obj(self.src_par.get_id(), self.src_par)

        self.assertRaises(TimDbException, ref_par.get_referenced_pars)
        self.assertRaises(TimDbException, self.src_par.get_referenced_pars)

        db.close()

    def test_transitive(self):
        db = self.init_testdb()

        # Reference to the original paragraph
        ref_par1 = self.ref_doc.add_ref_paragraph(self.src_par)
        self.assertEqual(1, len(self.ref_doc))
        self.assertEqual(ref_par1.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual('', ref_par1.get_markdown())

        # Reference to the reference above
        ref_doc2 = self.doc_create(db, "referencing reference", 3)
        ref_par2 = ref_doc2.add_ref_paragraph(ref_par1)
        self.assertEqual(1, len(ref_doc2))
        self.assertEqual(ref_par2.get_id(), ref_doc2.get_paragraphs()[0].get_id())
        self.assertEqual('', ref_par2.get_markdown())

        # Render the reference to the reference
        rendered_pars = ref_par2.get_referenced_pars()
        self.assertEqual(1, len(rendered_pars))
        self.assertEqual(self.src_par.get_id(), rendered_pars[0].get_id())
        self.assertEqual(self.src_par.get_markdown(), rendered_pars[0].get_markdown())
        self.assertEqual(self.src_par.get_html(), rendered_pars[0].get_html())
        self.assertEqual(self.src_par.get_attrs(), rendered_pars[0].get_attrs())
        self.assertEqual(self.src_par.get_properties(), rendered_pars[0].get_properties())

        # Declare some new attributes
        ref_par1.set_attr('foo', 'fffooo')
        ref_par2.set_attr('bar', 'baaaa')
        self.ref_doc.modify_paragraph_obj(ref_par1.get_id(), ref_par1)
        ref_doc2.modify_paragraph_obj(ref_par2.get_id(), ref_par2)

        expected_attrs = self.src_par.get_attrs()
        expected_attrs['foo'] = 'fffooo'
        expected_attrs['bar'] = 'baaaa'

        rendered_pars = ref_par2.get_referenced_pars()
        self.assert_dict_issubset(expected_attrs, rendered_pars[0].get_attrs())

        db.close()

    def test_editparagraph_cite(self):
        db = self.init_testdb()

        src_md = self.src_par.get_exported_markdown()
        self.assertRegex(src_md, '^#- *\\{([ab]="[21]" ?){2}\\}\ntestpar\n$')

        ref_par = self.ref_doc.add_ref_paragraph(self.src_par)
        self.assertEqual(1, len(self.ref_doc))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual('', ref_par.get_markdown())

        ref_md = ref_par.get_exported_markdown()
        ref_blocks = [DocParagraph.create(doc=ref_par.doc, md=par['md'], attrs=par.get('attrs'))
              for par in DocumentParser(ref_md).validate_structure(
                  is_whole_document=False).get_blocks()]

        self.assertEqual(1, len(ref_blocks))
        self.assertEqual('', ref_blocks[0].get_markdown())
        self.assertEqual(str(self.src_doc.doc_id), str(ref_blocks[0].get_attr('rd')))
        self.assertEqual(self.src_par.get_id(), ref_blocks[0].get_attr('rp'))
        self.assertEqual(self.src_par.get_hash(), ref_blocks[0].get_attr('rt'))

        db.close()

    def test_editparagraph_translate(self):
        db = self.init_testdb()

        src_md = self.src_par.get_exported_markdown()
        self.assertRegex(src_md, '^#- *\\{([ab]="[21]" ?){2}\\}\ntestpar\n$')

        empty_refpar = self.ref_doc.add_ref_paragraph(self.src_par, "")
        self.assertEqual(1, len(self.ref_doc))
        self.assertEqual(empty_refpar.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual("", empty_refpar.get_markdown())

        ref_md = empty_refpar.get_exported_markdown()
        ref_blocks = [DocParagraph.create(doc=empty_refpar.doc, md=par['md'], attrs=par.get('attrs'))
              for par in DocumentParser(ref_md).validate_structure(
                  is_whole_document=False).get_blocks()]

        self.assertEqual(1, len(ref_blocks))
        self.assertEqual(self.src_par.get_markdown(), ref_blocks[0].get_markdown())
        self.assertEqual(str(self.src_doc.doc_id), str(ref_blocks[0].get_attr('rd')))
        self.assertEqual(self.src_par.get_id(), ref_blocks[0].get_attr('rp'))
        self.assertEqual(self.src_par.get_hash(), ref_blocks[0].get_attr('rt'))
        self.assertEqual("tr", ref_blocks[0].get_attr('r'))


        ref_attrs = {'foo': 'fffoooo', 'bar': 'baaaa'}
        ref_par = self.ref_doc.add_ref_paragraph(self.src_par, "translation", attrs=ref_attrs)
        self.assertEqual(2, len(self.ref_doc))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[1].get_id())
        self.assertEqual("translation", ref_par.get_markdown())

        ref_md = ref_par.get_exported_markdown()
        ref_blocks = [DocParagraph.create(doc=ref_par.doc, md=par['md'], attrs=par.get('attrs'))
              for par in DocumentParser(ref_md).validate_structure(
                  is_whole_document=False).get_blocks()]

        self.assertEqual(1, len(ref_blocks))
        self.assertEqual(ref_par.get_markdown(), ref_blocks[0].get_markdown())
        self.assertEqual(str(self.src_doc.doc_id), str(ref_blocks[0].get_attr('rd')))
        self.assertEqual(self.src_par.get_id(), ref_blocks[0].get_attr('rp'))
        self.assertEqual(self.src_par.get_hash(), ref_blocks[0].get_attr('rt'))
        self.assertEqual("tr", ref_blocks[0].get_attr('r'))

        db.close()


if __name__ == '__main__':
    unittest.main()
