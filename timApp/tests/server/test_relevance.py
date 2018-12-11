from timApp.tests.server.timroutetest import TimRouteTest


class RelevanceTest(TimRouteTest):

    def test_get_relevance_none(self):
        self.login_test3()
        d = self.create_doc()
        self.get(f'/items/relevance/get/{d.id}', expect_content=None)

    def test_set_relevance_and_get(self):
        self.login_test3()
        d = self.create_doc()
        self.json_post(f'/items/relevance/set/{d.id}', {'value': 100})
        self.get(f'/items/relevance/get/{d.id}', expect_content={
            'default': False,
            'inherited': False,
            'relevance': {'block_id': d.id, 'relevance': 100}})

    def test_set_and_replace_relevance_and_get(self):
        self.login_test3()
        d = self.create_doc()
        self.json_post(f'/items/relevance/set/{d.id}', {'value': 100})
        self.json_post(f'/items/relevance/set/{d.id}', {'value': 50})
        self.get(f'/items/relevance/get/{d.id}', expect_content={
            'default': False,
            'inherited': False,
            'relevance': {'block_id': d.id, 'relevance': 50}})

    def test_set_relevance_no_manage_rights(self):
        self.login_test3()
        d = self.create_doc()
        self.login_test2()
        self.json_post(f'/items/relevance/set/{d.id}', {'value': 25}, expect_status=403)
