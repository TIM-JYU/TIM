from timApp.item.tag import TagType
from timApp.tests.server.timroutetest import TimRouteTest


class TagTest(TimRouteTest):

    def test_tag(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(f'/tags/add/{d.path}', {'tags': [{'name': 'test', 'expires': None, 'type': TagType.Regular},
                                                        {'name': 'test2', 'expires': None, 'type': TagType.Regular}]})
