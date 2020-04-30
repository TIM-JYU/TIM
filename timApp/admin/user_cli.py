import csv
from dataclasses import dataclass
from pprint import pprint
from typing import List, Optional, Set, Iterable

import click
from flask import abort
from flask.cli import AppGroup

from timApp.admin.import_accounts import import_accounts_impl
from timApp.auth.accesstype import AccessType
from timApp.document.docinfo import move_document
from timApp.tim_app import get_home_organization_group
from timApp.timdb.sqa import db
from timApp.user.personaluniquecode import SchacPersonalUniqueCode
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException


def create_user_info_set(u: User) -> Set[str]:
    """Returns a set of strings constructed from various parts of user info.
    This set is meant to be intersected with another user to determine whether they have anything in common.
    """
    real_name_ascii = u.real_name.translate(str.maketrans('åöäÅÖÄ', 'aoaAOA')).lower()
    return {
        u.name.lower(),
        *u.real_name.lower().split(' '),
        u.email_name_part.lower(),
        real_name_ascii.replace(' ', ''),
        ''.join(real_name_ascii.split(' ')[::-1])
    }


def has_anything_in_common(u1: User, u2: User) -> bool:
    u1_set = create_user_info_set(u1)
    u2_set = create_user_info_set(u2)
    if u1_set & u2_set:
        return True
    # This allows e.g. testuser1 and testuser2 to be merged.
    return bool(set(n[:-1] for n in u1_set) & set(n[:-1] for n in u2_set))


user_cli = AppGroup('user')


@user_cli.command()
@click.argument('name')
def addtohomeorg(name: str) -> None:
    """Adds a user to the home organization group.
    """
    u = User.get_by_name(name)
    if not u:
        click.echo('User not found.')
        return
    if u.is_email_user:
        click.echo('User is email user, so should not be added to home organization.')
        return
    if u.add_to_group(get_home_organization_group(), added_by=None):
        click.echo('Added.')
    else:
        click.echo('User already belongs to home organization.')
    db.session.commit()


@user_cli.command()
@click.argument('primary')
@click.argument('secondary')
def merge(primary: str, secondary: str) -> None:
    """Merges two users by moving data from secondary account to primary account,
    and soft-deletes the secondary account.
    """
    moved_data = find_and_merge_users(primary, secondary)
    find_and_soft_delete(secondary)
    db.session.commit()
    pprint(moved_data)


@dataclass
class MergeResult:
    primary: User
    secondary: User
    owned_lectures: int = 0
    lectureanswers: int = 0
    messages: int = 0
    answers: int = 0
    annotations: int = 0
    velps: int = 0
    readparagraphs: int = 0
    notes: int = 0
    accesses: int = 0


@user_cli.command()
@click.argument('csvfile', type=click.File())
def mass_merge(csvfile: Iterable[str]) -> None:
    """Merges multiple users as specified in the given CSV file.
    """
    reader = csv.reader(csvfile, delimiter=';')
    for row in reader:
        if len(row) != 2:
            raise click.UsageError(f'CSV file has a row with wrong number of columns: {row}')
        primary, secondary = row[0], row[1]
        result = find_and_merge_users(primary, secondary)
        click.echo(str(result))
        find_and_soft_delete(secondary)
    db.session.commit()


def find_and_merge_users(primary: str, secondary: str) -> MergeResult:
    u_prim = User.get_by_name(primary)
    u_sec = User.get_by_name(secondary)
    if not u_prim:
        return abort(404, f'User {primary} not found')
    if not u_sec:
        return abort(404, f'User {secondary} not found')
    return do_merge_users(u_prim, u_sec)


def do_merge_users(u_prim: User, u_sec: User) -> MergeResult:
    if u_prim.is_special:
        return abort(400, f'User {u_prim.name} is a special user')
    if u_sec.is_special:
        return abort(400, f'User {u_sec.name} is a special user')
    if u_prim == u_sec:
        return abort(400, 'Users cannot be the same')
    if not has_anything_in_common(u_prim, u_sec):
        return abort(400, f'Users {u_prim.name} and {u_sec.name} do not appear to be duplicates. '
                          f'Merging not allowed to prevent accidental errors.')
    moved_data = MergeResult(u_prim, u_sec)
    for a in ('owned_lectures', 'lectureanswers', 'messages', 'answers', 'annotations', 'velps'):
        a_alt = a + '_alt'
        setattr(moved_data, a, len(getattr(u_sec, a_alt)))
        getattr(u_prim, a_alt).extend(getattr(u_sec, a_alt))
        setattr(u_sec, a_alt, [])
    u_prim_group = u_prim.get_personal_group()
    u_sec_group = u_sec.get_personal_group()
    u_prim_folder = u_prim.get_personal_folder()
    u_sec_folder = u_sec.get_personal_folder()
    docs = u_sec_folder.get_all_documents(include_subdirs=True)
    for d in docs:
        move_document(d, u_prim_folder)
    for a in ('readparagraphs', 'notes', 'accesses'):
        a_alt = a + '_alt'
        setattr(moved_data, a, len(getattr(u_sec_group, a_alt)))
        if a == 'accesses':
            getattr(u_prim_group, a_alt).update(getattr(u_sec_group, a_alt))
            setattr(u_sec_group, a_alt, {})
        else:
            getattr(u_prim_group, a_alt).extend(getattr(u_sec_group, a_alt))
            setattr(u_sec_group, a_alt, [])
    # Restore ownership of secondary's personal folder:
    # * all users are allowed to have at most one personal folder
    # * if we don't restore access for secondary user, a new personal folder would be created when logging in
    for key, a in u_prim_group.accesses_alt.items():
        assert u_sec_folder.block is not None
        if a.block_id == u_sec_folder.block.id and a.type == AccessType.owner.value:
            moved_data.accesses -= 1
            u_prim_group.accesses_alt.pop(key)
            u_sec_group.accesses_alt[key] = a
            break
    return moved_data


@user_cli.command()
@click.argument('name')
def soft_delete(name: str) -> None:
    find_and_soft_delete(name)
    db.session.commit()


def find_and_soft_delete(name: str) -> None:
    u = User.get_by_name(name)
    if not u:
        raise RouteException('User not found.')
    do_soft_delete(u)


def do_soft_delete(u: User) -> None:
    d_suffix = '_deleted'
    if u.name.endswith(d_suffix) or u.email.endswith(d_suffix):
        raise RouteException('User is already soft-deleted.')
    u.update_info(UserInfo(username=u.name + d_suffix, email=u.email + d_suffix, full_name=u.real_name))


@user_cli.command()
@click.option('--username', prompt='Username')
@click.option('--firstname', prompt='First name', default='')
@click.option('--lastname', prompt='Last name', default='')
@click.option('--email', prompt='Email', default='')
@click.option('--password', prompt='Password', default='')
@click.option('--admin', default=False, is_flag=True, prompt='Make this user an administrator?')
def create(
        username: str,
        firstname: str,
        lastname: str,
        email: str,
        password: str,
        admin: bool,
) -> None:
    """Creates or updates a user."""

    user = User.query.filter_by(name=username).first()
    info = UserInfo(
        username=username,
        email=email or None,
        full_name=f'{lastname} {firstname}'.strip() or None,
        given_name=firstname or None,
        last_name=lastname or None,
        password=password or None,
    )
    if user:
        user.update_info(info)
        if admin:
            user.make_admin()
        click.echo('User updated.')
    else:
        User.create_with_group(info, is_admin=admin)
        click.echo('User created.')
    db.session.commit()


@user_cli.command()
def fix_aalto_student_ids() -> None:
    users_to_fix: List[User] = UserGroup.query.filter(
        UserGroup.name.in_(['aalto19test', 'cs-a1141-2017-2018'])).join(User, UserGroup.users).with_entities(
        User).all()
    for u in users_to_fix:
        u.set_unique_codes([SchacPersonalUniqueCode(
            code=u.name.split(':')[1],
            codetype='studentID',
            org='aalto.fi'
        )])
    db.session.commit()
    click.echo(f'Updated {len(users_to_fix)} users.')


@user_cli.command('import')
@click.option(
    '--csvfile',
    type=click.Path(exists=True),
    required=True,
    help='CSV file from which to read user accounts; format: email;full name;username',
)
@click.option(
    '--password',
    help='common password for all accounts',
)
def import_accounts(csvfile: str, password: Optional[str]) -> None:
    added, existing = import_accounts_impl(csvfile, password)
    total = len(added) + len(existing)
    click.echo(f'Processed {total} accounts.')
    if added:
        click.echo(f'Added the following {len(added)} accounts:')
    else:
        click.echo(f'No new accounts were added.')
    for u in added:
        click.echo(u.name)
    if existing:
        click.echo(f'Updated the following {len(existing)} existing accounts:')
    else:
        click.echo(f'No existing accounts were updated.')
    for u in existing:
        click.echo(u.name)
