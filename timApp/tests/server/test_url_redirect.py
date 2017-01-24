from urllib.parse import quote_plus

from tests.server.timroutetest import TimRouteTest


class RedirectTest(TimRouteTest):
    def test_redirect(self):
        self.login_test1()
        personal_folder = self.current_user.get_personal_folder().path
        testing_space = '{}/testing-space'.format(personal_folder)
        self.create_doc(testing_space)
        testing_space_cap = '{}/testing-spAce'.format(personal_folder)
        self.create_doc(testing_space_cap)
        testing_spoce = '{}/testing-spoce'.format(personal_folder)
        self.create_doc(testing_spoce)
        testing_remove = '{}/testingremove'.format(personal_folder)
        self.create_doc(testing_remove)
        for route in ('view', 'manage'):
            params = 'a=b'
            self.get('{}/{}/testing space'.format(route, personal_folder), expect_status=302,
                     expect_content='{}/{}'.format(route, testing_space))
            self.get('{}/{}/testing spAce'.format(route, personal_folder), expect_status=302,
                     expect_content='{}/{}'.format(route, testing_space_cap))
            self.get('{}/{}/testing späce'.format(route, personal_folder), expect_status=302,
                     expect_content='{}/{}?{}'.format(route, testing_space, params), query_string={'a': 'b'})
            self.get('{}/{}/testing spöce'.format(route, personal_folder), expect_status=302,
                     expect_content='{}/{}?{}'.format(route, testing_spoce, params), query_string={'a': 'b'})
            self.get('{}/{}/testing spåce'.format(route, personal_folder), expect_status=302,
                     expect_content='{}/{}?{}'.format(route, testing_space, params), query_string={'a': 'b'})
            for c in '<>|½!"#¤%&()=?`´¨~^\',.;:@£$€{[]}\\':
                self.get(quote_plus('{}/{}/testing{}remove'.format(route, personal_folder, c)),
                         expect_status=302,
                         expect_content='{}/{}?{}'.format(route, testing_remove, params), query_string={'a': 'b'})
