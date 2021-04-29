from unittest import TestCase

from timApp.util.utils import is_valid_email


class ValidEmailTest(TestCase):

    def assert_valid_email(self, email: str):
        self.assertTrue(is_valid_email(email))

    def assert_invalid_email(self, email: str):
        self.assertFalse(is_valid_email(email))

    def test_invalid_email(self):
        self.assert_invalid_email('')
        self.assert_invalid_email('@')
        self.assert_invalid_email('a@')
        self.assert_invalid_email('@a')
        self.assert_invalid_email('@a.com')
        self.assert_invalid_email('a@.com')
        self.assert_invalid_email('a@com')
        self.assert_invalid_email('a@a.b')
        self.assert_invalid_email('a@@a.com')
        self.assert_invalid_email('a@a@a.com')
        self.assert_invalid_email('a.@a.com')
        self.assert_invalid_email('.a@a.com')
        self.assert_invalid_email('a..a@a.com')
        self.assert_invalid_email('"@a.com')
        self.assert_invalid_email('a b@a.com')
        self.assert_invalid_email('a@a..aa')

        # TODO: These should be valid.
        self.assert_invalid_email('""@a.com')
        self.assert_invalid_email('"@"@a.com')
        self.assert_invalid_email('"a b"@a.com')
        self.assert_invalid_email("!#$%&'*+-/=?^_`{|}~@a.com")

    def test_valid_email(self):
        self.assert_valid_email('a+a+a@a.com')
        self.assert_valid_email('+a@a.com')
        self.assert_valid_email('a+@a.com')
        self.assert_valid_email('a@a.com')
        self.assert_valid_email('abc@xyz.com')
        self.assert_valid_email('a+a@a.com')
        self.assert_valid_email('abc+def@xyz.com')
        self.assert_valid_email('abc+def@xyz.fi')
        self.assert_valid_email('_@xyz.fi')
        self.assert_valid_email('-@xyz.fi')
        self.assert_valid_email('a-a@xyz.fi')
        self.assert_valid_email('a_a@xyz.fi')
        self.assert_valid_email('abc.def.ghi@xyz.fi')
        self.assert_valid_email('a@a.a.aa')

        # TODO: These should be invalid.
        self.assert_valid_email('a@-.--')
        self.assert_valid_email('a@a-.com')
        self.assert_valid_email('a@-a.com')
        self.assert_valid_email('a@a.-com')
        self.assert_valid_email('a@a.com-')
