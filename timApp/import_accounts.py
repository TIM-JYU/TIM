import csv
import sys

from tim_app import app
from timdb.timdb2 import TimDb


def import_accounts(file, password):
    timdb = TimDb(files_root_path=app.config['FILES_PATH'])
    with open(file) as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            timdb.users.create_user_with_group(row[0], row[1], row[0], password, is_admin=True, commit=False)
    timdb.commit()


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('Usage: import_accounts.py <CSV file> <password for all accounts>')
    else:
        import_accounts(sys.argv[1], sys.argv[2])
