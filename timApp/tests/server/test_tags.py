from timApp.tests.server.timroutetest import TimRouteTest


class TagTest(TimRouteTest):

    def test_tag(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(f'/tags/add/{d.path}', {'tags': ['test', 'test2'], 'expires': None})
