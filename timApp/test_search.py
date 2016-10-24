from timroutetest import TimRouteTest


class SearchTest(TimRouteTest):
    def test_search(self):
        self.login_test1()
        text_to_search = 'Text to search'
        self.create_doc(initial_par=text_to_search).document
        resp = self.get('/search/' + text_to_search, as_tree=True)
        pars = resp.findall('.//div[@class="par"]/div[@class="parContent"]/p')
        self.assertEqual(1, len(pars))
        self.assertEqual(text_to_search, pars[0].text)

    def test_failed_search(self):
        self.login_test1()
        self.get('/search/aa', expect_status=400)
