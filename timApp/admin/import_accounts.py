import csv
from typing import Tuple, List, Optional

from timApp.util.utils import is_valid_email
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.user.userutils import create_password_hash


class ImportException(Exception):
    pass


def import_accounts_impl(file: str, password: Optional[str]) -> Tuple[List[User], List[User]]:
    existing = []
    added = []
    with open(file) as csvfile:
        reader = csv.reader(csvfile, delimiter=';')
        pwhash = create_password_hash(password) if password else None
        for row in reader:
            if len(row) != 3:
                raise ImportException(f'All rows must have 3 fields, found a row with {len(row)} fields: {row}')
            email = row[0].lower()
            full_name = row[1]
            name = row[2]
            u = None
            if not email and not name:
                raise ImportException('Either name or email must be provided')
            if email:
                if not is_valid_email(email):
                    raise ImportException(f'Not a valid email: {email}')
            if name:
                u = User.get_by_name(name)
            if not u:
                if email:
                    users = User.get_by_email_case_insensitive(email)
                    if len(users) > 1:
                        raise ImportException(f'The email {email} matches multiple ({len(users)}) accounts.')
                    if users:
                        u = users[0]
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
    db.session.commit()
    return added, existing
