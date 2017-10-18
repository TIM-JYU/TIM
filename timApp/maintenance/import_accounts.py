import csv
import sys

from timApp.tim_app import app
from timApp.timdb.models.user import User
from timApp.timdb.timdb2 import TimDb


def import_accounts(file, password):
    timdb = TimDb(files_root_path=app.config['FILES_PATH'])
    existing = []
    with open(file) as csvfile:
        reader = csv.reader(csvfile, delimiter=';')
        for row in reader:
            if len(row) != 3:
                raise Exception(f'All rows must have 3 fields, found a row with {len(row)} fields: {row}')
            email = row[0]
            name = row[2] or email
            u = User.get_by_name(name)
            if u is None:
                u = User.get_by_email(email)
                if not u:
                    User.create_with_group(name=name,
                                           real_name=row[1],
                                           email=email,
                                           password=password,
                                           commit=False)
                else:
                    u.update_info(name, real_name=row[1], email=email, password=password)
                    existing.append(name)
            else:
                u.update_info(name, real_name=row[1], email=email, password=password)
                existing.append(name)
    timdb.commit()
    return existing


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('Usage: import_accounts.py <CSV file> <password for all accounts>')
    else:
        existing_accounts = import_accounts(sys.argv[1], sys.argv[2])
        if existing_accounts:
            print('The following accounts already exist and were skipped:')
            for e in existing_accounts:
                print(e)
