import responses
from flask import session

from timApp.routes.login import test_pws
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.models.usergroup import UserGroup


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

    @property
    def korppi_auth_url(self):
        return app.config['KORPPI_AUTHORIZE_URL']

    def test_korppi_signup(self):
        """Korppi signup succeeds."""
        auth_url = self.korppi_auth_url
        with responses.RequestsMock() as m:
            m.add('GET', auth_url,
                  body='')
            r = self.get('/korppiLogin', expect_status=303)
            self.assertEqual(r, f'{auth_url}?authorize={session["appcookie"]}&returnTo=http://localhost/korppiLogin')

        # ... user logs in with Korppi at this point ...

        with responses.RequestsMock() as m:
            m.add('GET', auth_url + '?request=' + session['appcookie'],
                  body='johmadoe\nDoe John Matt\njohn.m.doe@student.jyu.fi',
                  match_querystring=True)
            self.get('/korppiLogin', follow_redirects=True)
        self.assertEqual('Doe John Matt', self.current_user.real_name)
        self.assertEqual('johmadoe', self.current_user.name)
        self.assertEqual('john.m.doe@student.jyu.fi', self.current_user.email)
        self.assertEqual(list(g.name for g in self.current_user.groups.order_by(UserGroup.name)), ['johmadoe', 'Korppi users'])

    def test_korppi_info_change(self):
        """TIM can handle cases where some information about the user changes in Korppi."""
        auth_url = self.korppi_auth_url
        with responses.RequestsMock() as m:
            m.add('GET', auth_url,
                  body='johmadoe\nDoe John Matt\njohn.m.doe@student.jyu.fi')
            self.get('/korppiLogin', follow_redirects=True)
        curr_id = self.current_user.id
        curr_name = self.current_user.name
        curr_email = self.current_user.email

        # real name changes
        with responses.RequestsMock() as m:
            m.add('GET', auth_url,
                  body='johmadoe\nDoe John Matthew\njohn.m.doe@student.jyu.fi')
            self.get('/korppiLogin', follow_redirects=True)

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, curr_email)
        self.assertEqual(self.current_user.real_name, 'Doe John Matthew')

        # email changes
        with responses.RequestsMock() as m:
            m.add('GET', auth_url,
                  body='johmadoe\nDoe John Matthew\njohn.doe@student.jyu.fi')
            self.get('/korppiLogin', follow_redirects=True)

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, 'john.doe@student.jyu.fi')
        self.assertEqual(self.current_user.real_name, 'Doe John Matthew')

        # username changes
        with responses.RequestsMock() as m:
            m.add('GET', auth_url,
                  body='johmadoz\nDoe John Matthew\njohn.doe@student.jyu.fi')
            self.get('/korppiLogin', follow_redirects=True)

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, 'johmadoz')
        self.assertEqual(self.current_user.email, 'john.doe@student.jyu.fi')
        self.assertEqual(self.current_user.real_name, 'Doe John Matthew')
        self.assertEqual(list(g.name for g in self.current_user.groups.order_by(UserGroup.name)),
                         ['johmadoz', 'Korppi users'])

        # If both username and email is different, there's no way to identify the user.
        with responses.RequestsMock() as m:
            m.add('GET', auth_url,
                  body='johmadox\nDoe John Matthew\njohn.doex@student.jyu.fi')
            self.get('/korppiLogin', follow_redirects=True)
        self.assertNotEqual(self.current_user.id, curr_id)
