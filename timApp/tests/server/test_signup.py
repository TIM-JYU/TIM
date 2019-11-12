from flask import session

from timApp.auth.login import test_pws, create_or_update_user
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.newuser import NewUser
from timApp.user.user import User, UserOrigin, UserInfo
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import create_password_hash

test_pw = 'somepwd123'


class TestSignUp(TimRouteTest):
    def setUp(self):
        super().setUp()
        self.logout()

    def test_block_bot_signup(self):
        bot_email = 'bot@example.com'
        self.json_post(
            '/altsignup',
            {
                'email': bot_email,
                'url': 'http://www.example.com',
            })
        self.get('/')  # refresh session
        self.assertIsNone(NewUser.query.get(bot_email))

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
             'password': test_pw,
             'passconfirm': test_pw},
            expect_contains='registered',
            json_key='status')
        self.assertEqual(NewUser.query.with_entities(NewUser.email).all(), [])
        self.assertEqual('Testing Signup', self.current_user.real_name)
        self.assertEqual(UserOrigin.Email, self.current_user.origin)

        # TODO needs a better error message
        self.json_post(
            '/altsignup2',
            {'realname': 'Testing Signup',
             'token': test_pws[-1],
             'email': email,
             'password': test_pw,
             'passconfirm': test_pw},
            expect_content='Wrong temporary password. Please re-check your email to see the password.',
            expect_status=400,
        )

        self.json_post(
            '/altsignup',
            {'email': email})
        self.json_post(
            '/altsignup2',
            {'realname': 'Testing Signup2',
             'email': email,
             'token': test_pws[-1],
             'password': test_pw,
             'passconfirm': test_pw},
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
             'password': test_pw,
             'passconfirm': 'somepwd1232'},
            expect_content='Passwords do not match.',
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
            expect_content='A password should contain at least 10 characters.',
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
             'password': test_pw,
             'passconfirm': test_pw},
            expect_content='Wrong temporary password. Please re-check your email to see the password.',
            expect_status=400,
        )
        self.assertFalse(self.is_logged_in)

    def test_invalid_email(self):
        old_len = len(test_pws)
        self.json_post(
            '/altsignup',
            {'email': 'invalid'})
        self.assertFalse(self.is_logged_in)
        self.assertEqual(old_len, len(test_pws))

    def test_korppi_signup(self):
        """Korppi signup succeeds."""
        self.create_or_update_test_user(
            'johmadoenew',
            'Doe John Matt',
            'john.m.doenew@student.jyu.fi',
        )
        self.assertEqual('Doe John Matt', self.current_user.real_name)
        self.assertEqual('johmadoenew', self.current_user.name)
        self.assertEqual('john.m.doenew@student.jyu.fi', self.current_user.email)
        self.assertEqual(set(g.name for g in self.current_user.groups),
                         {'johmadoenew', 'Korppi users'})

    def create_or_update_test_user(self, username='johmadoe', real_name='Doe John Matt',
                                   email='john.m.doe@student.jyu.fi'):
        u = create_or_update_user(
            UserInfo(
                email=email,
                full_name=real_name,
                username=username,
                origin=UserOrigin.Korppi,
            ),
            group_to_add=UserGroup.get_korppi_group(),
        )
        db.session.commit()
        session['user_id'] = u.id

    def test_korppi_info_change(self):
        """TIM can handle cases where some information about the user changes in Korppi."""
        self.create_or_update_test_user()
        curr_id = self.current_user.id
        curr_name = self.current_user.name
        curr_email = self.current_user.email

        # real name changes
        self.create_or_update_test_user(
            real_name='Doe John Matthew',
            username='johmadoe',
            email='john.m.doe@student.jyu.fi',
        )

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, curr_email)
        self.assertEqual(self.current_user.real_name, 'Doe John Matthew')
        self.assertEqual(UserOrigin.Korppi, self.current_user.origin)

        # email changes
        self.create_or_update_test_user(
            real_name='Doe John Matthew',
            username='johmadoe',
            email='john.doe@student.jyu.fi',
        )

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, 'john.doe@student.jyu.fi')
        self.assertEqual(self.current_user.real_name, 'Doe John Matthew')

        # username changes
        self.create_or_update_test_user(
            real_name='Doe John Matthew',
            username='johmadoz',
            email='john.doe@student.jyu.fi',
        )

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, 'johmadoz')
        self.assertEqual(self.current_user.email, 'john.doe@student.jyu.fi')
        self.assertEqual(self.current_user.real_name, 'Doe John Matthew')
        self.assertEqual(set(g.name for g in self.current_user.groups),
                         {'johmadoz', 'Korppi users'})

        # If both username and email is different, there's no way to identify the user.
        self.create_or_update_test_user(
            real_name='Doe John Matthew',
            username='johmadox',
            email='john.doex@student.jyu.fi',
        )
        self.assertNotEqual(self.current_user.id, curr_id)

    def test_korppi_email_signup(self):
        """A Korppi user can update their password (and real name) by signing up."""
        self.create_or_update_test_user()
        curr_id = self.current_user.id
        curr_name = self.current_user.name
        curr_real_name = self.current_user.real_name
        curr_email = self.current_user.email
        self.json_post(
            '/altsignup',
            {'email': curr_email})
        pw = test_pw
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

        self.create_or_update_test_user()
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
        self.assertFalse(UserGroup.get_korppi_group() in self.current_user.groups)
        self.create_or_update_test_user('t3', 'Mr Test User 3', email=self.current_user.email)
        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, 't3')
        self.assertEqual(self.current_user.real_name, 'Mr Test User 3')
        self.assertEqual(self.current_user.pass_, curr_pw)
        self.assertTrue(UserGroup.get_korppi_group() in self.current_user.groups)

    def test_email_login_without_pass(self):
        self.create_or_update_test_user('someone', 'Some One', 'someone@example.com')
        u = User.get_by_name('someone')
        u.pass_ = None
        db.session.commit()
        self.login(email='someone@example.com', passw='something', force=True, expect_status=403)

    def test_email_login_with_korppi_username(self):
        self.create_or_update_test_user('someone2', 'Some One', 'someone2@example.com')
        u = User.get_by_name('someone2')
        u.pass_ = create_password_hash('somepass')
        db.session.commit()
        self.login(email='someone2', passw='somepass', force=True)

    def test_korppi_user_reset_pass_with_username(self):
        """A Korppi user can reset their password using their username."""
        self.create_or_update_test_user()
        curr_name = self.current_user.name
        self.json_post(
            '/altsignup',
            {'email': curr_name})
        pw = test_pw
        self.json_post(
            '/altsignup2',
            {'realname': 'Johnny John',
             'email': curr_name,
             'token': test_pws[-1],
             'password': pw,
             'passconfirm': pw},
            expect_contains='updated',
            json_key='status')

    def test_login_fail(self):
        basic_error = 'Email address or password did not match.'
        jyu_error = basic_error + ' You might not have a TIM account. JYU members can log in using Korppi.'
        self.login(email='a@example.com', passw='somepass', force=True,
                   expect_status=403,
                   expect_content=basic_error,
                   )
        self.login(email='a@jyu.fi', passw='somepass', force=True,
                   expect_status=403,
                   expect_content=jyu_error,
                   )
