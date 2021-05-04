from io import StringIO

import click

from timApp.admin.user_cli import do_import_passwords
from timApp.tests.db.timdbtest import TimDbTest


class PasswordImportTest(TimDbTest):
    def test_import_passwords(self):
        # Swap passwords of testuser1 and testuser2, and clear testuser3's password.
        names = """
test1@example.com;;$2b$04$B0mE/VeD5Uzucfa2juzY5.8aObzCqQSDVK//bxdiQ5Ayv59PwWsVq
test2@example.com;test1pass;$2b$04$zXpqPI7SNOWkbmYKb6QK9ePEUe.0pxZRctLybWNE1nxw0/WMiYlPu
test3@example.com;;
        """.strip()
        with self.suppress_stdout():
            do_import_passwords(StringIO(names), False)
        self.assertEqual('$2b$04$B0mE/VeD5Uzucfa2juzY5.8aObzCqQSDVK//bxdiQ5Ayv59PwWsVq', self.test_user_1.pass_)
        self.assertEqual('$2b$04$zXpqPI7SNOWkbmYKb6QK9ePEUe.0pxZRctLybWNE1nxw0/WMiYlPu', self.test_user_2.pass_)
        self.assertIsNone(self.test_user_3.pass_)

        wrong_password = 'test1@example.com;a;$2b$04$B0mE/VeD5Uzucfa2juzY5.8aObzCqQSDVK//bxdiQ5Ayv59PwWsVq'
        with self.assertRaises(click.UsageError):
            with self.suppress_stdout():
                do_import_passwords(StringIO(wrong_password), True)

        missing_hash = 'test1@example.com;a;'
        with self.assertRaises(click.UsageError):
            with self.suppress_stdout():
                do_import_passwords(StringIO(missing_hash), True)
