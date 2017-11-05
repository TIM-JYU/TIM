"""Tests for password hashing."""
import bcrypt

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.tim_models import db
from timApp.timdb.userutils import hash_password_old


class PasswordTest(TimRouteTest):
    def test_bcrypt_hash(self):
        self.login_test1()
        self.assertTrue(self.current_user.check_password('test1pass', allow_old=False))
        self.assertTrue(bcrypt.checkpw('test1pass'.encode(), self.current_user.pass_.encode()))

    def test_hash_migration(self):
        self.login_test2()
        self.current_user.pass_ = hash_password_old('test2pass')
        with self.assertRaises(ValueError):
            bcrypt.checkpw('test2pass'.encode(), self.current_user.pass_.encode())
        self.assertFalse(self.current_user.check_password('test2pass', allow_old=False))
        db.session.commit()
        self.logout()
        self.login_test2(force=True)
        self.login_test1()  # discard previous session to make sure the password hash update was persisted
        self.assertTrue(self.test_user_2.check_password('test2pass', allow_old=False))
        self.assertTrue(bcrypt.checkpw('test2pass'.encode(), self.test_user_2.pass_.encode()))
