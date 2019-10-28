import csv
import sys
from typing import Tuple, List

from timApp.tim_app import app
from timApp.user.user import User
from timApp.timdb.timdb import TimDb
from timApp.user.userutils import create_password_hash


def import_accounts(file: str, password: str) -> Tuple[List[User], List[User]]:
    timdb = TimDb(files_root_path=app.config['FILES_PATH'])
    existing = []
    added = []
    with open(file) as csvfile:
        reader = csv.reader(csvfile, delimiter=';')
        pwhash = create_password_hash(password)
        for row in reader:
            if len(row) != 3:
                raise Exception(f'All rows must have 3 fields, found a row with {len(row)} fields: {row}')
            email = row[0]
            name = row[2] or email
            u = User.get_by_name(name)
            if u is None:
                u = User.get_by_email(email)
                if not u:
                    u, _ = User.create_with_group(name=name,
                                                  real_name=row[1],
                                                  email=email,
                                                  password_hash=pwhash)
                    added.append(u)
                else:
                    u.update_info(name, real_name=row[1], email=email, password=password)
                    existing.append(u)
            else:
                u.update_info(name, real_name=row[1], email=email, password=password)
                existing.append(u)
    timdb.commit()
    return added, existing


def main():
    if len(sys.argv) < 3:
        print('Usage: import_accounts.py <CSV file> <password for all accounts>')
    else:
        added, existing = import_accounts(sys.argv[1], sys.argv[2])
        total = len(added) + len(existing)
        print(f'Processed {total} accounts.')
        if added:
            print(f'Added the following {len(added)} accounts:')
        else:
            print(f'No new accounts were added.')
        for u in added:
            print(u.email)
        if existing:
            print(f'Updated the following {len(existing)} existing accounts:')
        else:
            print(f'No existing accounts were updated.')
        for u in existing:
            print(u.email)


if __name__ == '__main__':
    main()
