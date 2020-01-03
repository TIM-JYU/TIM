from unittest.mock import patch, Mock

from timApp.auth.accesstype import AccessType
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.docsettings import DocSettings
from timApp.document.document import Document
from timApp.document.translation.translation import Translation
from timApp.document.yamlblock import YamlBlock
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.util.utils import EXAMPLE_DOCS_PATH


class TranslationTest(TimRouteTest):

    def test_translation_create(self):
        self.login_test1()
        doc = self.create_doc()
        lang = 'en'
        doc_title = 'test'
        t = self.create_translation(doc, doc_title, lang)
        self.create_translation(doc, doc_title, lang, expect_status=403,
                                expect_content={'error': 'Translation for this language already exists'})
        self.get(t.url)
        self.logout()
        self.json_post(f'/translate/{doc.id}/{lang}',
                       {'doc_title': doc_title},
                       expect_status=403)

    def test_translation_create_with_settings(self):
        self.login_test1()
        doc = self.create_doc()
        doc.document.set_settings({'a': 'b'})
        lang = 'en'
        doc_title = 'test'
        t = self.create_translation(doc, doc_title, lang)
        d = t.document
        self.assertEqual('b', d.get_settings().get_dict()['a'])
        self.get(t.url)

    def test_translation_content(self):
        self.login_test1()
        doc = self.create_doc(from_file=f'{EXAMPLE_DOCS_PATH}/multiple_mmcqs.md')
        j = self.create_translation(doc, 'MMCQ fi', 'fi')
        tr_doc = j.document
        pars = doc.document.get_paragraphs()
        par_ids = set(p.get_id() for p in pars)
        tr_pars = tr_doc.get_paragraphs()
        plugin_tr_par = tr_pars[1]
        old_md = self.get(f'/getBlock/{tr_doc.doc_id}/{plugin_tr_par.get_id()}')
        self.assertEqual({}, tr_doc.get_settings().get_dict())

        self.assertFalse(tr_pars[0].is_setting())
        for p in tr_pars: # type DocParagraph
            self.assertTrue(p.is_translation())
            self.assertTrue(p.get_attr('rp') is not None)
            self.assertIn(p.get_attr('rp'), par_ids)
        finnish = 'Vastaa kyllä tai ei seuraaviin'
        english = 'Answer yes or no to the following questions'
        new_md = old_md['text'].replace(english, finnish).replace('true', 'false')
        self.post_par(tr_doc, new_md, plugin_tr_par.get_id(),
                      expect_contains=finnish,
                      json_key='texts',
                      expect_xpath='.//mmcq')

        md = self.get(f'/getBlock/{tr_doc.doc_id}/{plugin_tr_par.get_id()}')
        self.assertEqual(new_md, md['text'])
        pars = doc.document.get_paragraphs()
        self.assertIn(english, pars[1].get_markdown())

        # make sure that the translated markdown is applied and not the original when answering from Finnish document
        task_id = f'{doc.id}.{pars[1].get_attr("taskId")}'
        task_id_ext = f'{task_id}.{pars[1].get_id()}'
        self.post_answer('mmcq',
                         task_id_ext,
                         [False, False, False],
                         ref_from=(tr_doc.doc_id, plugin_tr_par.get_id()))
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
        self.assertEqual([p.get_id() for p in doc.document.get_paragraphs()],
                         [p.get_attr('rp') for p in tr_doc.get_paragraphs()])

    def test_translation_sync(self):
        """Translations are kept in sync."""
        self.login_test1()
        doc = self.create_doc()
        tr = self.create_translation(doc)
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

        self.delete_par(doc, doc.document.get_paragraphs()[-1].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.delete_par(doc, doc.document.get_paragraphs()[0].get_id())
        self.assert_translation_synced(tr_doc, doc)

        self.update_whole_doc(doc, 'replaced all with this')
        self.assert_translation_synced(tr_doc, doc)

        self.update_whole_doc(doc, '')
        self.assert_translation_synced(tr_doc, doc)

    def test_translation_extraneous_pars(self):
        """Any extraneous blocks (those without "rp" attribute) in translation documents are retained after syncing."""
        self.login_test1()
        doc = self.create_doc(initial_par=['1', '2', '3', '4'])
        tr = self.create_translation(doc)
        tr_doc = tr.document
        self.assert_translation_synced(tr_doc, doc)
        tr_doc.insert_paragraph('new', insert_before_id=tr_doc.get_paragraphs()[1].get_id())
        tr_doc.add_paragraph('new2')

        self.delete_par(doc, doc.document.get_paragraphs()[0].get_id())
        tr_doc.clear_mem_cache()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual('new', tr_pars[0].get_markdown())
        self.assertEqual('new2', tr_pars[-1].get_markdown())
        self.assertEqual(doc.document.get_paragraphs()[0].get_id(), tr_pars[1].get_attr('rp'))

        self.update_whole_doc(doc, 'whole new text')
        doc.document.clear_mem_cache()
        tr_doc.clear_mem_cache()
        first_id = doc.document.get_paragraphs()[0].get_id()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual('new', tr_pars[0].get_markdown())
        self.assertEqual('new2', tr_pars[1].get_markdown())
        self.assertEqual(first_id, tr_pars[2].get_attr('rp'))

        self.new_par(doc.document, 'new first', first_id)
        tr_doc.clear_mem_cache()
        tr_pars = tr_doc.get_paragraphs()
        self.assertEqual('new', tr_pars[0].get_markdown())
        self.assertEqual('new2', tr_pars[1].get_markdown())
        pars = doc.document.get_paragraphs()
        self.assertEqual(pars[0].get_id(), tr_pars[2].get_attr('rp'))
        self.assertEqual(pars[1].get_id(), tr_pars[3].get_attr('rp'))

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

    def test_translation_settings(self):
        self.login_test1()
        d = self.create_doc(initial_par='hello')
        t = self.create_translation(d)
        d.document.set_settings({'a': 'b'})
        self.new_par(d.document, 'test')
        tr_pars = t.document.get_paragraphs()
        orig_pars = d.document.get_paragraphs()
        settings_id = orig_pars[0].get_id()
        self.assertEqual(tr_pars[0].get_attr('rp'), settings_id)
        tr_settings = DocSettings.from_paragraph(tr_pars[0])
        self.assertEqual(tr_settings.get_dict(), {})
        self.assertEqual(t.document.get_settings().get_dict(), {'a': 'b'})

        d.document.set_settings({'a': 'b', 'c': 'd'})
        tr_pars[0].set_markdown(YamlBlock(values={'c': 'x'}).to_markdown())
        tr_pars[0].save()
        self.assertEqual(t.document.get_settings().get_dict(), {'a': 'b', 'c': 'x'})

    def test_translation_ignored_src_doc(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        t = self.create_translation(d)
        fake_id = 9999
        t.document.set_settings({'source_document': fake_id})
        self.assertEqual(t.document.get_source_document().doc_id, d.id)
        self.assertNotEqual(d.id, fake_id)

    def test_translation_invalid(self):
        """Missing rp attribute will not crash the document."""
        self.login_test1()
        d = self.create_doc()
        t = self.create_translation(d)
        t.document.add_text('#- {r=tr}')
        self.get(t.url)

    def test_translation_outofdate(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
a
#-
b
        """)
        t = self.create_translation(d)
        self.check_outofdate_count(t, 2)
        par = d.document.get_paragraphs()[0]
        tr_par = t.document.get_paragraphs()[0]

        self.check_preview_diff(t, tr_par, '', 'a\n')
        self.post_par(t.document,
                      tr_par.get_exported_markdown() + ' tr',
                      tr_par.get_id(),
                      extra_data={'tags': {'marktranslated': True}})
        tr_par = t.document.get_paragraphs()[0]
        self.check_preview_diff(t, tr_par, 'a\n', 'a\n')
        self.check_outofdate_count(t, 1)
        self.post_par(d.document,
                      par.get_exported_markdown() + ' edit',
                      par.get_id())
        self.check_preview_diff(t, tr_par, 'a\n', 'a\n edit\n')
        self.check_outofdate_count(t, 2)
        self.post_par(t.document,
                      tr_par.get_exported_markdown() + ' tr2',
                      tr_par.get_id(),
                      extra_data={'tags': {'marktranslated': True}})
        self.check_outofdate_count(t, 1)
        self.post_par(t.document,
                      tr_par.get_exported_markdown() + ' tr',
                      tr_par.get_id(),
                      extra_data={'tags': {'marktranslated': False}})
        self.check_outofdate_count(t, 2)

        # Make sure merely toggling translation state (without changing markdown) will update the paragraph.
        self.post_par(t.document,
                      tr_par.get_exported_markdown() + ' tr',
                      tr_par.get_id(),
                      extra_data={'tags': {'marktranslated': True}})
        self.check_outofdate_count(t, 1)

        t = Translation.find_by_id(t.id)
        self.test_user_2.grant_access(t, AccessType.view)
        db.session.commit()

        # only editors should see the outofdate messages
        self.login_test2()
        self.check_outofdate_count(t, 0)

    def check_preview_diff(self, t, tr_par, old, new):
        r = self.post_preview(t, tr_par.get_exported_markdown(), par=tr_par.get_id(), json_key='trdiff')
        self.assertEqual(old, r['old'])
        self.assertEqual(new, r['new'])

    def check_outofdate_count(self, t, count):
        e = self.get(t.url, as_tree=True)
        outofdates = e.cssselect('.tr-outofdate')
        self.assertEqual(count, len(outofdates))

    def test_mark_all_translated(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
a
#-
b
#-
c
        """)
        t = self.create_translation(d)
        self.json_post(f'/markTranslated/{t.id}')
        self.check_outofdate_count(t, 0)
        self.assertEqual(6, len(t.document.get_changelog().entries))
        self.json_post(f'/markTranslated/{t.id}')
        t.document.clear_mem_cache()
        self.assertEqual(6, len(t.document.get_changelog().entries))
