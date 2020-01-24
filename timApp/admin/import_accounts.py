import csv
import sys
from typing import Tuple, List

from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.timdb import TimDb
from timApp.user.user import User, UserInfo
from timApp.user.userutils import create_password_hash


class ImportException(Exception):
    pass


def import_accounts(file: str, password: str) -> Tuple[List[User], List[User]]:
    timdb = TimDb(files_root_path=get_files_path())
    existing = []
    added = []
    with open(file) as csvfile:
        reader = csv.reader(csvfile, delimiter=';')
        pwhash = create_password_hash(password)
        for row in reader:
            if len(row) != 3:
                raise ImportException(f'All rows must have 3 fields, found a row with {len(row)} fields: {row}')
            email = row[0]
            full_name = row[1]
            name = row[2]
            u = None
            if not email and not name:
                raise ImportException('Either name or email must be provided')
            if name:
                u = User.get_by_name(name)
            if not u:
                if email:
                    u = User.get_by_email(email)
            if not full_name:
                raise ImportException('Full name missing')
            if u is None:
                u, _ = User.create_with_group(
                    UserInfo(
                        username=name or email,
                        full_name=full_name,
                        email=email or None,
                        password_hash=pwhash,
                    )
                )
                added.append(u)
            else:
                u.update_info(
                    UserInfo(username=name or None, full_name=full_name, email=email or None, password_hash=pwhash),
                )
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
            print(u.name)
        if existing:
            print(f'Updated the following {len(existing)} existing accounts:')
        else:
            print(f'No existing accounts were updated.')
        for u in existing:
            print(u.name)


if __name__ == '__main__':
    main()
