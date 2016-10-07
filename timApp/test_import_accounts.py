import csv
import os

from import_accounts import import_accounts
from timdbtest import TimDbTest


class AccountImportTest(TimDbTest):
    def test_import_accounts(self):
        accounts = [(name + '@example.com', name) for name in ['testimport{}'.format(i) for i in range(1, 100)]]
        csv_path = os.path.join(self.test_files_path, 'import.csv')
        with open(csv_path, 'w') as f:
            w = csv.writer(f)
            for a in accounts:
                w.writerow(a)
        import_accounts(csv_path, 'testpass')
        db = self.get_db()
        for a in accounts:
            self.assertEqual(a[1], db.users.get_user_by_email(a[0])['real_name'])
