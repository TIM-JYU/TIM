from routes.login import test_pws
from tests.server.timroutetest import TimRouteTest


class TestSignUp(TimRouteTest):
    def setUp(self):
        super().setUp()
        self.logout()

    def test_signup(self):
        self.post('/altsignup',
                  data={'email': 'testingsignup@example.com'},
                  follow_redirects=True,
                  expect_contains='A password has been sent to you. Please check your email.')
        self.post('/altsignup2',
                  data={'realname': 'Testing Signup',
                        'token': test_pws[-1],
                        'password': 'somepwd',
                        'passconfirm': 'somepwd'},
                  follow_redirects=True,
                  xhr=False,
                  expect_contains='Registration succeeded!')
        self.assertEqual('Testing Signup', self.current_user.real_name)

        # TODO needs a better error message
        self.post('/altsignup2',
                  data={'realname': 'Testing Signup',
                        'token': test_pws[-1],
                        'password': 'somepwd',
                        'passconfirm': 'somepwd'},
                  follow_redirects=True,
                  xhr=False,
                  expect_contains='The temporary password you provided is wrong. Please re-check your email to see the password.')

        self.post('/altsignup',
                  data={'email': 'testingsignup@example.com'},
                  follow_redirects=True,
                  expect_contains='A password has been sent to you. Please check your email.')
        self.post('/altsignup2',
                  data={'realname': 'Testing Signup2',
                        'token': test_pws[-1],
                        'password': 'somepwd',
                        'passconfirm': 'somepwd'},
                  follow_redirects=True,
                  xhr=False,
                  expect_contains='Your information was updated successfully.')
        self.assertEqual('Testing Signup2', self.current_user.real_name)

    def test_password_mismatch(self):
        self.post('/altsignup',
                  data={'email': 'testingsignup@example.com'},
                  follow_redirects=True)
        self.post('/altsignup2',
                  data={'realname': 'Testing Signup',
                        'token': test_pws[-1],
                        'password': 'somepwd',
                        'passconfirm': 'somepwd2'},
                  follow_redirects=True,
                  expect_contains='Passwords do not match.')
        self.assertFalse(self.is_logged_in)

    def test_too_short_password(self):
        self.post('/altsignup',
                  data={'email': 'testingsignup@example.com'},
                  follow_redirects=True)
        self.post('/altsignup2',
                  data={'realname': 'Testing Signup',
                        'token': test_pws[-1],
                        'password': 'test',
                        'passconfirm': 'test'},
                  follow_redirects=True,
                  expect_contains='A password should contain at least six characters.')
        self.assertFalse(self.is_logged_in)

    def test_temp_password_wrong(self):
        self.post('/altsignup',
                  data={'email': 'testingsignup@example.com'},
                  follow_redirects=True)
        self.post('/altsignup2',
                  data={'realname': 'Testing Signup',
                        'token': 'asdasd',
                        'password': 'somepwd',
                        'passconfirm': 'somepwd'},
                  follow_redirects=True,
                  expect_contains='The temporary password you provided is wrong. Please re-check your email to see the password.',
                  )
        self.assertFalse(self.is_logged_in)

    def test_invalid_email(self):
        self.post('/altsignup',
                  data={'email': 'invalid'},
                  follow_redirects=True,
                  expect_contains='You must supply a valid email address!')
        self.assertFalse(self.is_logged_in)

    def todo_test_korppi_signup(self):
        """Tests that Korppi signup succeeds.

        Does not currently work; needs a (separate) server if Flask TestClient is not enough, as it appears.

        """
        self.get('/korppiLogin', follow_redirects=True)
        self.assertEqual('Doe John Matt', self.current_user.real_name)
