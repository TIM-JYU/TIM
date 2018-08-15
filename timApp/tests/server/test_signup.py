import responses
from flask import session

from timApp.auth.login import test_pws
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.user.newuser import NewUser
from timApp.user.user import User
from timApp.timdb.sqa import db


class TestSignUp(TimRouteTest):
    def setUp(self):
        super().setUp()
        self.logout()

    def test_signup(self):
        email = 'testingsignup@example.com'
        self.json_post(
            '/altsignup',
            {'email': email})
        self.assertEqual(NewUser.query.with_entities(NewUser.email).all(), [(email,)])
        self.json_post(
            '/altsignup',
            {'email': email})
        self.assertEqual(NewUser.query.with_entities(NewUser.email).all(), [(email,)])
        self.json_post(
            '/altsignup2',
            {'realname': 'Testing Signup',
             'email': email,
             'token': test_pws[-1],
             'password': 'somepwd',
             'passconfirm': 'somepwd'},
            expect_contains='registered',
            json_key='status')
        self.assertEqual(NewUser.query.with_entities(NewUser.email).all(), [])
        self.assertEqual('Testing Signup', self.current_user.real_name)

        # TODO needs a better error message
        self.json_post(
            '/altsignup2',
            {'realname': 'Testing Signup',
             'token': test_pws[-1],
             'email': email,
             'password': 'somepwd',
             'passconfirm': 'somepwd'},
            expect_contains='Wrong temporary password. Please re-check your email to see the password.',
            expect_status=400,
            json_key='error')

        self.json_post(
            '/altsignup',
            {'email': email})
        self.json_post(
            '/altsignup2',
            {'realname': 'Testing Signup2',
             'email': email,
             'token': test_pws[-1],
             'password': 'somepwd',
             'passconfirm': 'somepwd'},
            expect_contains='updated',
            json_key='status')
        self.assertEqual('Testing Signup2', self.current_user.real_name)

    def test_password_mismatch(self):
        email = 'testingsignup@example.com'
        self.json_post(
            '/altsignup',
            {'email': email})
        self.json_post(
            '/altsignup2',
            {'realname': 'Testing Signup',
             'email': email,
             'token': test_pws[-1],
             'password': 'somepwd',
             'passconfirm': 'somepwd2'},
            expect_contains='Passwords do not match.',
            json_key='error',
            expect_status=400)
        self.assertFalse(self.is_logged_in)

    def test_too_short_password(self):
        email = 'testingsignup@example.com'
        self.json_post(
            '/altsignup',
            {'email': email})
        self.json_post(
            '/altsignup2',
            {'realname': 'Testing Signup',
             'email': email,
             'token': test_pws[-1],
             'password': 'test',
             'passconfirm': 'test'},
            expect_contains='A password should contain at least six characters.',
            json_key='error',
            expect_status=400,
        )
        self.assertFalse(self.is_logged_in)

    def test_temp_password_wrong(self):
        email = 'testingsignup@example.com'
        self.json_post(
            '/altsignup',
            {'email': email})
        self.json_post(
            '/altsignup2',
            {'realname': 'Testing Signup',
             'email': email,
             'token': 'asdasd',
             'password': 'somepwd',
             'passconfirm': 'somepwd'},
            expect_contains='Wrong temporary password. Please re-check your email to see the password.',
            json_key='error',
            expect_status=400,
        )
        self.assertFalse(self.is_logged_in)

    def test_invalid_email(self):
        self.json_post(
            '/altsignup',
            {'email': 'invalid'},
            expect_contains='Email address is not valid',
            json_key='error',
            expect_status=400)
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
        self.json_post(
            '/altsignup',
            {'email': curr_email})
        pw = 'somepwd'
        self.json_post(
            '/altsignup2',
            {'realname': 'Johnny John',
             'email': curr_email,
             'token': test_pws[-1],
             'password': pw,
             'passconfirm': pw},
            expect_contains='updated',
            json_key='status')
        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, curr_email)
        self.assertEqual(self.current_user.real_name, 'Doe John Matt')  # changing name not allowed for Korppi users
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
