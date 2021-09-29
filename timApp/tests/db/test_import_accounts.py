import csv
import os

from timApp.admin.import_accounts import import_accounts_impl, ImportException
from timApp.tests.db.timdbtest import TimDbTest
from timApp.user.user import User, UserInfo


class AccountImportTest(TimDbTest):

    def test_import_accounts(self):
        num_accounts = 3
        accounts = [
            UserInfo(email=f'{name}@example.com', full_name=name, username=f'{name}uname')
            for name in [f'testimport{i}' for i in range(0, num_accounts)]]
        self.assertEqual(num_accounts, len(accounts))
        self.write_and_test(accounts)
        self.write_and_test(accounts, expected_existing=(name.username for name in accounts))
        accounts = [UserInfo(email=f'{name}@example.com', full_name=name, username='') for name in
                    [f'testimport_2_{i}' for i in range(0, num_accounts)]]
        self.assertEqual(num_accounts, len(accounts))
        self.write_and_test(accounts, username_is_email=True)

    def test_email_existing(self):
        accounts = [UserInfo(email='test1@example.com', full_name='Real Name 1', username='t1')]
        self.write_and_test(accounts, expected_existing=(name.username for name in accounts))

    def test_no_email(self):
        accounts = [
            UserInfo(email='', full_name='Real Name 1', username='x1'),
            UserInfo(email='', full_name='Real Name 2', username='x2'),
        ]
        self.write_and_test(accounts)
        self.write_and_test(accounts, expected_existing=(name.username for name in accounts))

    def test_email_and_name_missing(self):
        accounts = [
            UserInfo(email='', full_name='Real Name 1', username=''),
        ]
        with self.assertRaises(ImportException) as e:
            self.write_and_test(accounts)
        self.assertEqual('Either name or email must be provided', e.exception.args[0])

    def write_and_test(self, accounts: list[UserInfo], username_is_email=False, expected_existing=None):
        if expected_existing is None:
            expected_existing = []
        csv_path = self.write_test_csv(accounts)
        _, existing = import_accounts_impl(csv_path, 'testpass')
        self.assertEqual(set(expected_existing), {u.name for u in existing})
        for a in accounts:
            if a.email:
                u = User.get_by_email(a.email)
                expected_email = a.email
            else:
                u = User.get_by_name(a.username)
                expected_email = None
            self.assertEqual(a.full_name, u.real_name)
            self.assertEqual(expected_email, u.email)
            if username_is_email:
                self.assertEqual(a.email, u.name)
            else:
                self.assertEqual(a.username, u.name)

    def write_test_csv(self, accounts, include_username=True):
        csv_path = os.path.join(self.test_files_path, 'import.csv')
        with open(csv_path, 'w') as f:
            w = csv.writer(f, delimiter=';')
            for a in accounts:
                row_data = [a.email, a.full_name]
                if include_username:
                    row_data.append(a.username)
                else:
                    row_data.append('')
                w.writerow(row_data)
        return csv_path

    def test_import_no_password(self):
        csv_path = self.write_test_csv([UserInfo(full_name='Testing User', username='t@example.com')])
        added, _ = import_accounts_impl(csv_path, '')
        self.assertIsNone(added[0].pass_)

    def test_import_existing_no_name(self):
        csv_path = self.write_test_csv(
            [UserInfo(full_name='Testing User 2', email='t2@example.com')],
            include_username=False)
        added, _ = import_accounts_impl(csv_path, '')
        _, existing = import_accounts_impl(csv_path, '')

    def test_import_lowercase(self):
        csv_path = self.write_test_csv(
            [UserInfo(full_name='Testing User 3', email='T3@example.com')],
            include_username=False)
        import_accounts_impl(csv_path, '')
        self.assertIsNone(User.get_by_email('T3@example.com'))
        self.assertIsNotNone(User.get_by_email('t3@example.com'))

    def test_invalid_email(self):
        accounts = [
            UserInfo(email='x', full_name='Real Name 1', username=''),
        ]
        with self.assertRaises(ImportException) as e:
            self.write_and_test(accounts)
        self.assertEqual('Not a valid email: x', e.exception.args[0])
