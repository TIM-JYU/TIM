from timApp.auth.login import is_possibly_home_org_account
from timApp.tests.server.timroutetest import TimRouteTest


class JyuAccountTest(TimRouteTest):
    def test_is_jyu_account(self):
        self.login_test1()
        self.assertTrue(is_possibly_home_org_account('a@jyu.fi'))
        self.assertTrue(is_possibly_home_org_account('a@student.jyu.fi'))
        self.assertTrue(is_possibly_home_org_account('a@cc.jyu.fi'))
        self.assertTrue(is_possibly_home_org_account('a@nice.domain.jyu.fi'))
        self.assertTrue(is_possibly_home_org_account('john'))

    def test_is_not_jyu_account(self):
        self.login_test1()
        self.assertFalse(is_possibly_home_org_account('a@example.com'))
        self.assertFalse(is_possibly_home_org_account('jyu.fi'))
        self.assertFalse(is_possibly_home_org_account('verylongname'))
