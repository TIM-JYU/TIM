"""Unit tests for Clipboard class.

Run from parent directory with command:
python3 -m unittest dumboclient filemodehelper documentmodel/test_clipboard.py
"""

from documentmodel.clipboard import Clipboard
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
        doc = db.documents.create('Lähdedokumentti', db.users.get_anon_group_id())

        pars = [doc.add_paragraph('Kappale {}'.format(i), attrs={'kappale': str(i)}) for i in range(0, 10)]

        clip = self.clipboard.get(1)
        clip.copy_pars(doc, pars[3].get_id(), pars[6].get_id())

        read_pars = clip.read()
        self.assertEqual(len(read_pars), 4)
        for i in range(3, 7):
            self.assertEqual(read_pars[i-3]['md'], pars[i].get_markdown())
            self.assertDictEqual(read_pars[i-3]['attrs'], pars[i].get_attrs())

    def test_paste(self):
        clip = self.clipboard.get(1)

        pars = [{'id': random_id(), 'md': 'Kappale 1.{}'.format(i), 'attrs': {'kappale': str(i)}} for i in range(0, 1)]
        clip.write(pars)

        db = self.get_db()
        doc = db.documents.create('Kohdedokumentti', db.users.get_anon_group_id())
        dest_pars = [doc.add_paragraph('Kohdekappale {}'.format(i), attrs={'kkappale': str(i)}) for i in range(0, 10)]

        ver_before = doc.get_version()
        clip.paste_before(doc, dest_pars[0].get_id())
        self.assertEqual(doc.get_version(), (ver_before[0] + 1, 0))

        new_pars = doc.get_paragraphs()
        self.assertEqual(len(new_pars), 11)
        self.assertEqual(new_pars[0].get_markdown(), pars[0]['md'])
        self.assertEqual(new_pars[1].get_markdown(), dest_pars[0].get_markdown())

        pars = [{'id': random_id(), 'md': 'Kappale 2.{}'.format(i), 'attrs': {'kappale': str(i)}} for i in range(0, 3)]
        clip.write(pars)

        ver_before = doc.get_version()
        clip.paste_before(doc, new_pars[2].get_id())
        self.assertEqual(doc.get_version(), (ver_before[0] + 3, 0))

        new_new_pars = doc.get_paragraphs()
        self.assertEqual(len(new_new_pars), 14)
        self.assertEqual(new_new_pars[0].get_markdown(), new_pars[0].get_markdown())
        self.assertEqual(new_new_pars[1].get_markdown(), new_pars[1].get_markdown())
        self.assertEqual(new_new_pars[2].get_markdown(), pars[0]['md'])
        self.assertEqual(new_new_pars[3].get_markdown(), pars[1]['md'])
        self.assertEqual(new_new_pars[4].get_markdown(), pars[2]['md'])
        self.assertEqual(new_new_pars[5].get_markdown(), new_pars[2].get_markdown())

        ver_before = doc.get_version()
        clip.paste_before(doc, None)
        self.assertEqual(doc.get_version(), (ver_before[0] + 3, 0))

        final_pars = doc.get_paragraphs()
        self.assertEqual(len(final_pars), 17)
        self.assertEqual(final_pars[13].get_markdown(), new_new_pars[13].get_markdown())
        self.assertEqual(final_pars[14].get_markdown(), pars[0]['md'])
        self.assertEqual(final_pars[15].get_markdown(), pars[1]['md'])
        self.assertEqual(final_pars[16].get_markdown(), pars[2]['md'])


def test_paste_ref(self):
    clip = self.clipboard.get(1)
    db = self.get_db()

    src_doc = db.documents.create('Lähdedokumentti', 2)
    dest_doc = db.documents.create('Kohdedokumentti', 1)
    src_pars = [src_doc.add_paragraph('Lähdekappale {}'.format(i), attrs={'lkappale': str(i)}) for i in range(0, 3)]
    dest_pars = [dest_doc.add_paragraph('Kohdekappale {}'.format(i), attrs={'kkappale': str(i)}) for i in range(0, 5)]

    # Single paragraph from index 0 to index 0
    ver_before = dest_doc.get_version()
    clip.copy_pars(src_doc, src_pars[0].get_id(), src_pars[0].get_id())
    clip.paste_before(dest_doc, dest_pars[0].get_id(), as_ref=True)
    self.assertEqual(dest_doc.get_version(), (ver_before[0] + 1, 0))

    new_pars = dest_doc.get_paragraphs()
    self.assertEqual(len(new_pars), 11)
    self.assertEqual(new_pars[0].get_attr('rd'), str(src_doc.doc_id))
    self.assertEqual(new_pars[0].get_attr('rp'), src_pars[0].get_id())
    self.assertEqual(new_pars[0].get_attr('ra'), None)
    self.assertEqual(new_pars[1].get_markdown(), src_pars[0].get_markdown())
    self.assertEqual(new_pars[1].get_attr('rd'), None)
    self.assertEqual(new_pars[1].get_attr('rp'), None)
    self.assertEqual(new_pars[1].get_attr('ra'), None)

    # 2 paragraphs from index 1 to index 2
    ver_before = dest_doc.get_version()
    clip.copy_pars(src_doc, src_pars[1].get_id(), src_pars[2].get_id())
    clip.paste_before(dest_doc, dest_pars[2].get_id(), as_ref=True)
    self.assertEqual(dest_doc.get_version(), (ver_before[0] + 2, 0))

    new_pars2 = dest_doc.get_paragraphs()
    self.assertEqual(len(new_pars2), len(new_pars) + 2)
    self.assertEqual(new_pars2[0].get_attr('rd'), str(src_doc.doc_id))
    self.assertEqual(new_pars2[0].get_attr('rp'), src_pars[0].get_id())
    self.assertEqual(new_pars2[0].get_attr('ra'), None)
    self.assertEqual(new_pars2[1].get_attr('rd'), str(src_doc.doc_id))
    self.assertEqual(new_pars2[1].get_attr('rp'), src_pars[1].get_id())
    self.assertEqual(new_pars2[1].get_attr('ra'), None)
    self.assertEqual(new_pars2[2].get_attr('rd'), str(src_doc.doc_id))
    self.assertEqual(new_pars2[2].get_attr('rp'), src_pars[2].get_id())
    self.assertEqual(new_pars2[2].get_attr('ra'), None)
    self.assertEqual(new_pars2[3].get_markdown(), src_pars[0].get_markdown())
