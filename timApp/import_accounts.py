import csv
import sys

from tim_app import app
from timdb.timdb2 import TimDb


def import_accounts(file, password):
    timdb = TimDb(files_root_path=app.config['FILES_PATH'])
    with open(file) as csvfile:
        reader = csv.reader(csvfile, delimiter=';')
        for row in reader:
            if len(row) != 3:
                raise Exception('All rows must have 3 fields, found a row with {} fields: {}'.format(len(row), row))
            timdb.users.create_user_with_group(name=row[2] or row[0],
                                               real_name=row[1],
                                               email=row[0],
                                               password=password,
                                               commit=False)
    timdb.commit()


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('Usage: import_accounts.py <CSV file> <password for all accounts>')
    else:
        import_accounts(sys.argv[1], sys.argv[2])
