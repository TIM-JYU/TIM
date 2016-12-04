from documentmodel.document import Document
from tests.server.timroutetest import TimRouteTest


class TranslationTest(TimRouteTest):
    def test_translation_create(self):
        self.login_test1()
        doc = self.create_doc()
        lang = 'en'
        doc_title = 'test'
        j = self.create_translation(doc, doc_title, lang)
        self.get('/view/{}'.format(j['name']))
        self.logout()
        self.json_post('/translate/{}/{}'.format(doc.id, lang),
                       {'doc_title': doc_title},
                       expect_status=403)

    def create_translation(self, doc, doc_title, lang):
        j = self.json_post('/translate/{}/{}'.format(doc.id, lang),
                           {'doc_title': doc_title},
                           expect_contains={'title': doc_title, 'name': doc.name + '/' + lang})
        return j

    def test_translation_content(self):
        self.login_test1()
        doc = self.create_doc(from_file='example_docs/multiple_mmcqs.md')
        j = self.create_translation(doc, 'MMCQ fi', 'fi')
        tr_doc = Document(j['id'])
        pars = doc.document.get_paragraphs()
        par_ids = set(p.get_id() for p in pars)
        tr_pars = tr_doc.get_paragraphs()
        old_md = self.get('/getBlock/{}/{}'.format(tr_doc.doc_id, tr_pars[2].get_id()))
        self.assertDictEqual({'macros': {}, 'source_document': doc.id}, tr_doc.get_settings().get_dict())

        # all but the settings paragraph are translated paragraphs
        self.assertTrue(tr_pars[0].is_setting())
        for p in tr_pars[1:]:
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
