from unittest.mock import patch, Mock

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import Document
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.docinfo import DocInfo


class TranslationTest(TimRouteTest):

    def test_translation_create(self):
        self.login_test1()
        doc = self.create_doc()
        lang = 'en'
        doc_title = 'test'
        j = self.create_translation(doc, doc_title, lang)
        self.create_translation(doc, doc_title, lang, expect_status=403,
                                expect_content={'error': 'Translation for this language already exists'})
        self.get('/view/{}'.format(j.path))
        self.logout()
        self.json_post('/translate/{}/{}'.format(doc.id, lang),
                       {'doc_title': doc_title},
                       expect_status=403)

    def test_translation_create_with_settings(self):
        self.login_test1()
        doc = self.create_doc()
        doc.document.set_settings({'a': 'b'})
        lang = 'en'
        doc_title = 'test'
        j = self.create_translation(doc, doc_title, lang)
        d = Document(j.id)
        self.assertEqual('b', d.get_settings().get_dict()['a'])
        self.get('/view/{}'.format(j.path))

    def test_translation_content(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/multiple_mmcqs.md')
        j = self.create_translation(doc, 'MMCQ fi', 'fi')
        tr_doc = j.document
        pars = doc.document.get_paragraphs()
        par_ids = set(p.get_id() for p in pars)
        tr_pars = tr_doc.get_paragraphs()
        old_md = self.get('/getBlock/{}/{}'.format(tr_doc.doc_id, tr_pars[2].get_id()))
        self.assertDictEqual({'source_document': doc.id}, tr_doc.get_settings().get_dict())

        # all but the settings paragraph are translated paragraphs
        self.assertTrue(tr_pars[0].is_setting())
        for p in tr_pars[1:]: # type DocParagraph
            self.assertTrue(p.is_translation())
            self.assertTrue(p.get_attr('rp') is not None)
            self.assertIn(p.get_attr('rp'), par_ids)
        finnish = 'Vastaa kyllä tai ei seuraaviin'
        english = 'Answer yes or no to the following questions'
        new_md = old_md['text'].replace(english, finnish).replace('true', 'false')
        self.post_par(tr_doc, new_md, tr_pars[2].get_id(),
                      expect_contains=finnish,
                      json_key='texts',
                      expect_xpath='.//mmcq')

        md = self.get('/getBlock/{}/{}'.format(tr_doc.doc_id, tr_pars[2].get_id()))
        self.assertEqual(new_md, md['text'])
        pars = doc.document.get_paragraphs()
        self.assertIn(english, pars[1].get_markdown())

        # make sure that the translated markdown is applied and not the original when answering from Finnish document
        task_id = '{}.{}'.format(doc.id, pars[1].get_attr('taskId'))
        task_id_ext = '{}.{}'.format(task_id, pars[1].get_id())
        self.post_answer('mmcq',
                         task_id_ext,
                         [False, False, False],
                         ref_from=(tr_doc.doc_id, tr_pars[2].get_id()))
        data = self.get_task_answers(task_id)
        self.assertEqual(3.0, data[0]['points'])

        # make sure the English version is not affected
        self.post_answer('mmcq',
                         task_id_ext,
                         [False, True, True])
        data = self.get_task_answers(task_id)
        self.assertEqual(1.0, data[0]['points'])
        self.assertEqual(3.0, data[1]['points'])

    def assert_translation_synced(self, tr_doc: Document, doc: DocInfo):
        tr_doc.clear_mem_cache()
        doc.document.clear_mem_cache()
        self.assertEqual([None] + [p.get_id() for p in doc.document.get_paragraphs()],
                         [p.get_attr('rp') for p in tr_doc.get_paragraphs()])

    def test_translation_sync(self):
        """Translations are kept in sync."""
        self.login_test1()
        doc = self.create_doc()
        tr = self.create_translation(doc, 'In English', 'en')
        tr_doc = tr.document
        self.assert_translation_synced(tr_doc, doc)

        self.new_par(doc.document, '1')
        self.assert_translation_synced(tr_doc, doc)

        self.new_par(doc.document, '2')
        self.assert_translation_synced(tr_doc, doc)

        self.new_par(doc.document, '3', doc.document.get_paragraphs()[0].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.new_par(doc.document, '4', doc.document.get_paragraphs()[1].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.post_par(doc.document, '5\n#-\n6', doc.document.get_paragraphs()[-1].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.delete_par(doc.document, doc.document.get_paragraphs()[-1].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.delete_par(doc.document, doc.document.get_paragraphs()[0].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.update_whole_doc(doc.document, 'replaced all with this')
        self.assert_translation_synced(tr_doc, doc)

        self.update_whole_doc(doc.document, '')
        self.assert_translation_synced(tr_doc, doc)

    def test_translation_extraneous_pars(self):
        """Any extraneous blocks (those without "rp" attribute) in translation documents are retained after syncing."""
        self.login_test1()
        doc = self.create_doc(initial_par='1\n#-\n2\n#-\n3\n#-\n4')
        tr = self.create_translation(doc, 'In English', 'en')
        tr_doc = tr.document
        self.assert_translation_synced(tr_doc, doc)
        tr_doc.insert_paragraph('new', insert_before_id=tr_doc.get_paragraphs()[1].get_id())
        tr_doc.add_paragraph('new2')

        self.delete_par(doc.document, doc.document.get_paragraphs()[0].get_id())
        tr_doc.clear_mem_cache()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual('new', tr_pars[1].get_markdown())
        self.assertEqual('new2', tr_pars[-1].get_markdown())
        self.assertEqual(doc.document.get_paragraphs()[0].get_id(), tr_pars[2].get_attr('rp'))

        self.update_whole_doc(doc.document, 'whole new text')
        doc.document.clear_mem_cache()
        tr_doc.clear_mem_cache()
        first_id = doc.document.get_paragraphs()[0].get_id()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual('new', tr_pars[1].get_markdown())
        self.assertEqual('new2', tr_pars[2].get_markdown())
        self.assertEqual(first_id, tr_pars[3].get_attr('rp'))

        self.new_par(doc.document, 'new first', first_id)
        tr_doc.clear_mem_cache()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual('new', tr_pars[1].get_markdown())
        self.assertEqual('new2', tr_pars[2].get_markdown())
        pars = doc.document.get_paragraphs()
        self.assertEqual(pars[0].get_id(), tr_pars[3].get_attr('rp'))
        self.assertEqual(pars[1].get_id(), tr_pars[4].get_attr('rp'))

    def test_translation_perf(self):
        self.login_test1()
        doc = self.create_doc(initial_par='hello')
        lang = 'en'
        doc_title = 'test'
        tr = self.create_translation(doc, doc_title, lang)
        self.get(tr.url)

        # Cache should be fresh at this point, so __write should not be called.
        with patch.object(DocParagraph, '_DocParagraph__write') as m:  # type: Mock
            self.get(tr.url)
        m.assert_not_called()

    def test_translation_no_settings_sync(self):
        """The settings paragraph from the original document isn't copied to the new one."""
        self.login_test1()
        d = self.create_doc(initial_par='hello')
        t = self.create_translation(d, 'title', 'en')
        d.document.set_settings({'a': 'b'})
        self.new_par(d.document, 'test')
        tr_pars = t.document.get_paragraphs()
        orig_pars = d.document.get_paragraphs()
        settings_id = orig_pars[0].get_id()
        self.assertFalse(any(p.get_attr('rp') == settings_id for p in tr_pars))
