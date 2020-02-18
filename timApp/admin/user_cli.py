from pprint import pprint

import click
from flask import abort
from flask.cli import AppGroup

from timApp.auth.accesstype import AccessType
from timApp.document.docinfo import move_document
from timApp.tim_app import app, get_home_organization_group
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.util.flask.requesthelper import RouteException


def create_user_info_set(u: User):
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


def has_anything_in_common(u1: User, u2: User):
    u1_set = create_user_info_set(u1)
    u2_set = create_user_info_set(u2)
    if u1_set & u2_set:
        return True
    # This allows e.g. testuser1 and testuser2 to be merged.
    return bool(set(n[:-1] for n in u1_set) & set(n[:-1] for n in u2_set))


user_cli = AppGroup('user')


@user_cli.command('addtohomeorg')
@click.argument('name')
def add_to_jyu(name: str):
    """Adds a user to the home organization group.
    """
    with app.test_request_context():
        u = User.get_by_name(name)
        if not u:
            print('User not found.')
            return
        if u.is_email_user:
            print('User is email user, so should not be added to home organization.')
            return
        if u.add_to_group(get_home_organization_group(), added_by=None):
            print('Added.')
        else:
            print('User already belongs to home organization.')
        db.session.commit()


@user_cli.command('merge')
@click.argument('primary')
@click.argument('secondary')
def merge_users(primary, secondary):
    """Merges two users by moving data from secondary account to primary account.

    This does not delete accounts.
    """
    with app.test_request_context():
        moved_data = find_and_merge_users(primary, secondary)
        db.session.commit()
    pprint(moved_data)
    return moved_data


def find_and_merge_users(primary: str, secondary: str):
    u_prim = User.get_by_name(primary)
    u_sec = User.get_by_name(secondary)
    if not u_prim:
        return abort(404, f'User {primary} not found')
    if not u_sec:
        return abort(404, f'User {secondary} not found')
    return do_merge_users(u_prim, u_sec)


def do_merge_users(u_prim: User, u_sec: User):
    if u_prim.is_special:
        return abort(400, f'User {u_prim.name} is a special user')
    if u_sec.is_special:
        return abort(400, f'User {u_sec.name} is a special user')
    if u_prim == u_sec:
        return abort(400, 'Users cannot be the same')
    if not has_anything_in_common(u_prim, u_sec):
        return abort(400, f'Users {u_prim.name} and {u_sec.name} do not appear to be duplicates. '
                          f'Merging not allowed to prevent accidental errors.')
    moved_data = {}
    for a in ('owned_lectures', 'lectureanswers', 'messages', 'answers', 'annotations', 'velps'):
        a_alt = a + '_alt'
        moved_data[a] = len(getattr(u_sec, a_alt))
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
        moved_data[a] = len(getattr(u_sec_group, a_alt))
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
        if a.block_id == u_sec_folder.block.id and a.type == AccessType.owner.value:
            moved_data['accesses'] -= 1
            u_prim_group.accesses_alt.pop(key)
            u_sec_group.accesses_alt[key] = a
            break
    return moved_data


@user_cli.command('soft_delete')
@click.argument('name')
def soft_delete(name: str):
    find_and_soft_delete(name)


def find_and_soft_delete(name: str):
    u = User.get_by_name(name)
    if not u:
        raise RouteException('User not found.')
    do_soft_delete(u)
    db.session.commit()


def do_soft_delete(u: User):
    d_suffix = '_deleted'
    if u.name.endswith(d_suffix) or u.email.endswith(d_suffix):
        raise RouteException('User is already soft-deleted.')
    u.update_info(UserInfo(username=u.name + d_suffix, email=u.email + d_suffix, full_name=u.real_name))
