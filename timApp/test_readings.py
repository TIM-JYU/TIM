from timroutetest import TimRouteTest


class ReadingsTest(TimRouteTest):
    def test_readings_normal(self):
        self.login_test1()
        doc = self.create_doc(initial_par='test\n#-\ntest2')
        par_id = doc.get_paragraphs()[0].get_id()
        readline_xpath = './/div[@class="readline"]'
        readlines = self.get('/view/{}'.format(doc.doc_id), as_tree=True).findall(readline_xpath)
        self.assertEqual(2, len(readlines))
        self.json_put('/read/{}/{}'.format(doc.doc_id, par_id))
        readlines = self.get('/view/{}'.format(doc.doc_id), as_tree=True).findall(readline_xpath)
        self.assertEqual(1, len(readlines))
