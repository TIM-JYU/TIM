import responses
from flask import session

from timApp.routes.login import test_pws
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.timdb.models.newuser import NewUser
from timApp.timdb.models.user import User
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.tim_models import db


class TestSignUp(TimRouteTest):
    def setUp(self):
        super().setUp()
        self.logout()

    def test_signup(self):
        self.post('/altsignup',
                  data={'email': 'testingsignup@example.com'},
                  follow_redirects=True,
                  expect_contains='A password has been sent to you. Please check your email.')
        self.assertEqual(NewUser.query.with_entities(NewUser.email).all(), [('testingsignup@example.com',)])
        self.post('/altsignup',
                  data={'email': 'testingsignup@example.com'},
                  follow_redirects=True,
                  expect_contains='A password has been sent to you. Please check your email.')
        self.assertEqual(NewUser.query.with_entities(NewUser.email).all(), [('testingsignup@example.com',)])
        self.post('/altsignup2',
                  data={'realname': 'Testing Signup',
                        'token': test_pws[-1],
                        'password': 'somepwd',
                        'passconfirm': 'somepwd'},
                  follow_redirects=True,
                  xhr=False,
                  expect_contains='Registration succeeded!')
        self.assertEqual(NewUser.query.with_entities(NewUser.email).all(), [])
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
                  body='johmadoenew\nDoe John Matt\njohn.m.doenew@student.jyu.fi',
                  match_querystring=True)
            self.get('/korppiLogin', follow_redirects=True)
        self.assertEqual('Doe John Matt', self.current_user.real_name)
        self.assertEqual('johmadoenew', self.current_user.name)
        self.assertEqual('john.m.doenew@student.jyu.fi', self.current_user.email)
        self.assertEqual(set(g.name for g in self.current_user.groups),
                         {'johmadoenew', 'Korppi users'})

    def register_user_with_korppi(self, username='johmadoe', real_name='Doe John Matt',
                                  email='john.m.doe@student.jyu.fi'):
        auth_url = self.korppi_auth_url
        with responses.RequestsMock() as m:
            m.add('GET', auth_url,
                  body=f'{username}\n{real_name}\n{email}')
            self.get('/korppiLogin', follow_redirects=True)

    def test_korppi_info_change(self):
        """TIM can handle cases where some information about the user changes in Korppi."""
        auth_url = self.korppi_auth_url
        self.register_user_with_korppi()
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
        self.assertEqual(set(g.name for g in self.current_user.groups),
                         {'johmadoz', 'Korppi users'})

        # If both username and email is different, there's no way to identify the user.
        with responses.RequestsMock() as m:
            m.add('GET', auth_url,
                  body='johmadox\nDoe John Matthew\njohn.doex@student.jyu.fi')
            self.get('/korppiLogin', follow_redirects=True)
        self.assertNotEqual(self.current_user.id, curr_id)

    def test_korppi_email_signup(self):
        """A Korppi user can update their password (and real name) by signing up."""
        self.register_user_with_korppi()
        curr_id = self.current_user.id
        curr_name = self.current_user.name
        curr_real_name = self.current_user.real_name
        curr_email = self.current_user.email
        self.post('/altsignup',
                  data={'email': curr_email},
                  follow_redirects=True,
                  expect_contains='A password has been sent to you. Please check your email.')
        pw = 'somepwd'
        self.post('/altsignup2',
                  data={'realname': 'Johnny John',
                        'token': test_pws[-1],
                        'password': pw,
                        'passconfirm': pw},
                  follow_redirects=True,
                  xhr=False,
                  expect_contains='Your information was updated successfully.')
        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, curr_email)
        self.assertEqual(self.current_user.real_name, 'Johnny John')
        self.assertTrue(self.current_user.check_password(pw))

        self.logout()
        self.assertIsNone(self.current_user)
        self.login(email=curr_email, passw=pw, force=True)
        self.assertEqual(self.current_user.id, curr_id)

        self.register_user_with_korppi()
        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, curr_email)
        self.assertEqual(self.current_user.real_name, curr_real_name)
        self.assertTrue(self.current_user.check_password(pw))

    def test_email_user_to_korppi(self):
        """When an email user logs in with Korppi, no new account is created but the current account information is updated."""
        self.login_test3()
        curr_id = self.current_user.id
        curr_pw = self.current_user.pass_
        self.register_user_with_korppi('t3', 'Mr Test User 3', email=self.current_user.email)
        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, 't3')
        self.assertEqual(self.current_user.real_name, 'Mr Test User 3')
        self.assertEqual(self.current_user.pass_, curr_pw)

    def test_email_login_without_pass(self):
        self.register_user_with_korppi('someone', 'Some One', 'someone@example.com')
        u = User.get_by_name('someone')
        u.pass_ = None
        db.session.commit()
        self.login(email='someone@example.com', passw='something', force=True, expect_status=403)
