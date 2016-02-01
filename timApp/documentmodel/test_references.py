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
    def test_simpleref(self):
        db = self.get_db()
        src_doc = db.documents.create("original", 1)
        ref_doc = db.documents.create("referencing", 2)

        src_par = src_doc.add_paragraph("testpar", attrs={"a": "1", "b": "2"}, properties={"p_a": "a", "p_b": "b"})
        self.assertEqual(1, len(src_doc))
        self.assertEqual(src_par.get_id(), src_doc.get_paragraphs()[0].get_id())

        ref_par = ref_doc.add_ref_paragraph(src_par)
        self.assertEqual(1, len(ref_doc))
        self.assertEqual(ref_par.get_id(), ref_doc.get_paragraphs()[0].get_id())

        deref_pars = ref_par.get_referenced_pars()
        self.assertEqual(1, len(deref_pars))
        self.assertEqual(src_par.get_id(), deref_pars[0].get_id())
        self.assertEqual(src_par.get_markdown(), deref_pars[0].get_markdown())
        self.assertEqual(src_par.get_html(), deref_pars[0].get_html())
        self.assertEqual(src_par.get_attrs(), deref_pars[0].get_attrs())
        self.assertEqual(src_par.get_properties(), deref_pars[0].get_properties())

if __name__ == '__main__':
    unittest.main()
