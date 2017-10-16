import csv
import os

from timApp.import_accounts import import_accounts
from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.models.user import User


class AccountImportTest(TimDbTest):

    def test_import_accounts(self):
        num_accounts = 10
        accounts = [(name + '@example.com', name, name + 'uname') for name in
                    [f'testimport{i}' for i in range(0, num_accounts)]]
        self.assertEqual(num_accounts, len(accounts))
        self.write_and_test(accounts)
        self.write_and_test(accounts, expected_existing=(name for _, _, name in accounts))
        accounts = [(name + '@example.com', name, '') for name in
                    [f'testimport_2_{i}' for i in range(0, num_accounts)]]
        self.assertEqual(num_accounts, len(accounts))
        self.write_and_test(accounts, username_is_email=True)

    def test_email_existing(self):
        accounts = [('test1@example.com', 'Real Name 1', 't1')]
        self.write_and_test(accounts, expected_existing=(name for _, _, name in accounts))

    def write_and_test(self, accounts, username_is_email=False, expected_existing=None):
        if expected_existing is None:
            expected_existing = []
        csv_path = os.path.join(self.test_files_path, 'import.csv')
        with open(csv_path, 'w') as f:
            w = csv.writer(f, delimiter=';')
            for a in accounts:
                w.writerow(a)
        existing = import_accounts(csv_path, 'testpass')
        self.assertSetEqual(set(expected_existing), set(existing))
        for a in accounts:
            u = User.get_by_email(a[0])
            self.assertEqual(a[1], u.real_name)
            self.assertEqual(a[0], u.email)
            if username_is_email:
                self.assertEqual(a[0], u.name)
            else:
                self.assertEqual(a[2], u.name)
