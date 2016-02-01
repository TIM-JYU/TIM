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

    def test_translation(self):
        db = self.init_testdb()

        ref_par = self.ref_doc.add_ref_paragraph(self.src_par, "translation")
        self.assertEqual(1, len(self.ref_doc))
        self.assertEqual(ref_par.get_id(), self.ref_doc.get_paragraphs()[0].get_id())
        self.assertEqual("translation", ref_par.get_markdown())

        rendered_pars = ref_par.get_referenced_pars()
        self.assertEqual(1, len(rendered_pars))
        self.assertEqual(self.src_par.get_id(), rendered_pars[0].get_id())
        self.assertEqual(ref_par.get_markdown(), rendered_pars[0].get_markdown())
        self.assertEqual(ref_par.get_html(), rendered_pars[0].get_html())
        self.assertEqual(self.dict_merge(self.src_par.get_attrs(), ref_par.get_attrs()), rendered_pars[0].get_attrs())
        self.assertEqual(self.dict_merge(self.src_par.get_properties(), ref_par.get_properties()),
                         rendered_pars[0].get_properties())

if __name__ == '__main__':
    unittest.main()
