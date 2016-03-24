"""Unit tests for Clipboard class.

Run from parent directory with command:
python3 -m unittest dumboclient filemodehelper documentmodel/test_clipboard.py
"""

import random
import unittest

from documentmodel.clipboard import Clipboard
from documentmodel.document import Document
from documentmodel.docparagraph import DocParagraph
from documentmodel.documentparser import DocumentParser
from documentmodel.documentwriter import DocumentWriter
from documentmodel.exceptions import DocExistsError
from documentmodel.randutils import random_paragraph
from documentmodel.randutils import random_id, hashfunc
from timdbtest import TimDbTest


class ClipboardTest(TimDbTest):
    def setUp(self):
        db = self.get_db()
        self.clipboard = Clipboard(db.files_root_path)
        self.clipboard.clear_all()

    def test_empty(self):
        clip = self.clipboard.get(1)
        self.assertIsNone(clip.read())

    def test_readwrite(self):
        text = 'kappale tekstiä'
        attrs = {'a': '1', 'b': '2'}
        par = {'id': random_id(), 'md': text, 't': hashfunc(text, attrs), 'attrs': attrs}

        clip = self.clipboard.get(1)
        clip.write([par])
        read_pars = clip.read()

        self.assertEqual(len(read_pars), 1)
        self.assertEqual(read_pars[0]['md'], par['md'])
        self.assertDictEqual(read_pars[0]['attrs'], par['attrs'])

    def test_persistence(self):
        text = 'kappale tekstiä'
        attrs = {'a': '1', 'b': '2'}
        par = {'id': random_id(), 'md': text, 't': hashfunc(text, attrs), 'attrs': attrs}

        clip = self.clipboard.get(1)
        clip.write([par])

        read_pars = clip.read()
        self.assertEqual(len(read_pars), 1)
        self.assertEqual(read_pars[0]['md'], par['md'])
        self.assertDictEqual(read_pars[0]['attrs'], par['attrs'])

        read_pars = clip.read()
        self.assertEqual(len(read_pars), 1)
        self.assertEqual(read_pars[0]['md'], par['md'])
        self.assertDictEqual(read_pars[0]['attrs'], par['attrs'])

        clip = self.clipboard.get(2)
        self.assertIsNone(clip.read())

        clip = self.clipboard.get(1)
        read_pars = clip.read()
        self.assertEqual(len(read_pars), 1)
        self.assertEqual(read_pars[0]['md'], par['md'])
        self.assertDictEqual(read_pars[0]['attrs'], par['attrs'])

    def test_copy(self):
        db = self.get_db()
        doc = db.documents.create('Lähdedokumentti', 1)

        pars = [doc.add_paragraph('Kappale {}'.format(i), attrs={'kappale': str(i)}) for i in range(0, 10)]

        clip = self.clipboard.get(1)
        clip.copy_pars(doc, pars[3].get_id(), pars[6].get_id())

        read_pars = clip.read()
        self.assertEqual(len(read_pars), 4)
        for i in range(3, 7):
            self.assertEqual(read_pars[i-3]['md'], pars[i].get_markdown())
            self.assertDictEqual(read_pars[i-3]['attrs'], pars[i].get_attrs())

