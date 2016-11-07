import csv
import os

from import_accounts import import_accounts
from timdbtest import TimDbTest


class AccountImportTest(TimDbTest):
    def test_import_accounts(self):
        num_accounts = 100
        accounts = [(name + '@example.com', name, name + 'uname') for name in
                    ['testimport{}'.format(i) for i in range(0, num_accounts)]]
        self.assertEqual(num_accounts, len(accounts))
        self.write_and_test(accounts)
        accounts = [(name + '@example.com', name, '') for name in
                    ['testimport_2_{}'.format(i) for i in range(0, num_accounts)]]
        self.assertEqual(num_accounts, len(accounts))
        self.write_and_test(accounts, username_is_email=True)

    def write_and_test(self, accounts, username_is_email=False):
        csv_path = os.path.join(self.test_files_path, 'import.csv')
        with open(csv_path, 'w') as f:
            w = csv.writer(f, delimiter=';')
            for a in accounts:
                w.writerow(a)
        import_accounts(csv_path, 'testpass')
        db = self.get_db()
        for a in accounts:
            u = db.users.get_user_by_email(a[0])
            self.assertEqual(a[1], u['real_name'])
            self.assertEqual(a[0], u['email'])
            if username_is_email:
                self.assertEqual(a[0], u['name'])
            else:
                self.assertEqual(a[2], u['name'])
