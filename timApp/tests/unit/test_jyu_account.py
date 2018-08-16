import unittest

from timApp.auth.login import is_possibly_jyu_account


class JyuAccountTest(unittest.TestCase):
    def test_is_jyu_account(self):
        self.assertTrue(is_possibly_jyu_account('a@jyu.fi'))
        self.assertTrue(is_possibly_jyu_account('a@student.jyu.fi'))
        self.assertTrue(is_possibly_jyu_account('a@cc.jyu.fi'))
        self.assertTrue(is_possibly_jyu_account('a@nice.domain.jyu.fi'))
        self.assertTrue(is_possibly_jyu_account('john'))

    def test_is_not_jyu_account(self):
        self.assertFalse(is_possibly_jyu_account('a@example.com'))
        self.assertFalse(is_possibly_jyu_account('jyu.fi'))
        self.assertFalse(is_possibly_jyu_account('verylongname'))
