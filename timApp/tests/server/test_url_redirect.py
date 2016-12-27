from urllib.parse import quote_plus

from tests.server.timroutetest import TimRouteTest


class RedirectTest(TimRouteTest):
    def test_redirect(self):
        self.login_test1()
        testing_space = 'users/testuser1/testing-space'
        self.create_doc(testing_space)
        testing_space_cap = 'users/testuser1/testing-spAce'
        self.create_doc(testing_space_cap)
        testing_spoce = 'users/testuser1/testing-spoce'
        self.create_doc(testing_spoce)
        testing_remove = 'users/testuser1/testingremove'
        self.create_doc(testing_remove)
        for route in ('view', 'manage'):
            params = 'a=b'
            self.get('{}/users/testuser1/testing space'.format(route), expect_status=302,
                     expect_content='{}/{}'.format(route, testing_space))
            self.get('{}/users/testuser1/testing spAce'.format(route), expect_status=302,
                     expect_content='{}/{}'.format(route, testing_space_cap))
            self.get('{}/users/testuser1/testing späce'.format(route), expect_status=302,
                     expect_content='{}/{}?{}'.format(route, testing_space, params), query_string={'a': 'b'})
            self.get('{}/users/testuser1/testing spöce'.format(route), expect_status=302,
                     expect_content='{}/{}?{}'.format(route, testing_spoce, params), query_string={'a': 'b'})
            self.get('{}/users/testuser1/testing spåce'.format(route), expect_status=302,
                     expect_content='{}/{}?{}'.format(route, testing_space, params), query_string={'a': 'b'})
            for c in '<>|½!"#¤%&()=?`´¨~^\',.;:@£$€{[]}\\':
                self.get(quote_plus('{}/users/testuser1/testing{}remove'.format(route, c)),
                         expect_status=302,
                         expect_content='{}/{}?{}'.format(route, testing_remove, params), query_string={'a': 'b'})
