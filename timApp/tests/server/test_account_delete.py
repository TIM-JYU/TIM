from timApp.tests.server.timroutetest import TimRouteTest


class AccountDeleteTest(TimRouteTest):
    def test_delete_account(self):
        self.login_test1()
        self.json_post('/settings/account/delete', {})
        self.assertFalse(self.is_logged_in)
        r = self.get('/')
        self.assertIn('Your account has been deleted.', r)
        with self.assertRaises(Exception):
            self.login_test1()
