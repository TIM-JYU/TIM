from timroutetest import TimRouteTest


class TranslationTest(TimRouteTest):
    def test_translation_create(self):
        self.login_test1()
        doc = self.create_doc()
        lang = 'en'
        doc_title = 'test'
        j = self.json_post('/translate/{}/{}'.format(doc.id, lang),
                           {'doc_title': doc_title},
                           expect_status=200,
                           as_json=True,
                           expect_content={'title': doc_title, 'name': doc.name + '/' + lang, 'id': doc.id + 2})
        self.get('/view/{}'.format(j['name']), expect_status=200)
        self.logout()
        self.json_post('/translate/{}/{}'.format(doc.id, lang),
                       {'doc_title': doc_title},
                       expect_status=403)
