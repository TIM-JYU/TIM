from typing import Tuple, List

from flask import Blueprint, abort

from timApp.accesshelper import verify_admin
from timApp.dbaccess import get_timdb
from timApp.responsehelper import json_response, ok_response
from timApp.timdb.models.user import User
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.special_group_names import SPECIAL_GROUPS

groups = Blueprint('groups',
                   __name__,
                   url_prefix='/groups')


USER_NOT_FOUND = 'User not found'
USERGROUP_NOT_FOUND = 'User group not found'


def get_uid_gid(groupname, usernames) -> Tuple[UserGroup, List[User]]:
    users = User.query.filter(User.name.in_(usernames)).all()
    group = UserGroup.query.filter_by(name=groupname).first()
    if group is None:
        abort(404, USERGROUP_NOT_FOUND)
    return group, users


@groups.route('/show/<groupname>')
def show_members(groupname):
    verify_admin()
    ug = UserGroup.get_by_name(groupname)
    if not ug:
        abort(404, USERGROUP_NOT_FOUND)
    return json_response(ug.users.all())


@groups.route('/usergroups/<username>')
def show_usergroups(username):
    verify_admin()
    u = User.get_by_name(username)
    if not u:
        abort(404, USER_NOT_FOUND)
    return json_response(u.get_groups(include_special=False).order_by(UserGroup.name).all())


@groups.route('/belongs/<username>/<groupname>')
def belongs(username, groupname):
    verify_admin()
    u = User.get_by_name(username)
    if not u:
        abort(404, USER_NOT_FOUND)
    ug = UserGroup.get_by_name(groupname)
    if not ug:
        abort(404, USERGROUP_NOT_FOUND)
    return json_response({'status': ug in u.groups})


@groups.route('/create/<groupname>')
def create_group(groupname):
    """Route for creating a usergroup.

    The usergroup name has the following restrictions:

     1. The name must have at least one digit.
     2. The name must have at least one alphabetic character.
     3. The name must NOT have any non-alphanumeric characters, with the exception that spaces are allowed.

    These restrictions are needed in order to distinguish manually-created groups from personal usergroups.
    Personal usergroup names are either

     1. email addresses (containing '@' character), or
     2. lowercase ASCII strings (Korppi users) with length being in range [2,8].

    """
    verify_admin()
    if UserGroup.get_by_name(groupname):
        abort(400, 'User group already exists.')
    has_digits = False
    has_letters = False
    has_non_alnum = False
    for c in groupname:
        has_digits = has_digits or c.isdigit()
        has_letters = has_letters or c.isalpha()
        has_non_alnum = has_non_alnum or not (c.isalnum() or c.isspace())
    if not has_digits or not has_letters or has_non_alnum:
        abort(400, 'Usergroup must contain at least one digit and one letter and must be alphanumeric.')
    UserGroup.create(groupname)
    return ok_response()


@groups.route('/addmember/<groupname>/<usernames>')
def add_member(usernames, groupname):
    verify_admin()
    if groupname in SPECIAL_GROUPS:
        abort(400, 'Cannot add members to special groups.')
    timdb = get_timdb()
    usernames = get_usernames(usernames)
    group, users = get_uid_gid(groupname, usernames)
    existing_usernames = set(u.name for u in users)
    existing_ids = set(u.id for u in group.users)
    already_exists = set(u.name for u in group.users) & set(usernames)
    not_exist = [name for name in usernames if name not in existing_usernames]
    added = []
    for u in users:
        if u.id not in existing_ids:
            u.groups.append(group)
            added.append(u.name)
    timdb.commit()
    return json_response({'already_belongs': sorted(list(already_exists)), 'added': added, 'not_exist': not_exist})


@groups.route('/removemember/<groupname>/<usernames>')
def remove_member(usernames, groupname):
    verify_admin()
    if groupname in SPECIAL_GROUPS:
        abort(400, 'Cannot remove members from special groups.')
    timdb = get_timdb()
    usernames = get_usernames(usernames)
    group, users = get_uid_gid(groupname, usernames)
    existing_usernames = set(u.name for u in users)
    existing_ids = set(u.id for u in group.users)
    not_exist = [name for name in usernames if name not in existing_usernames]
    removed = []
    does_not_belong = []
    for u in users:
        if u.id not in existing_ids:
            does_not_belong.append(u.name)
            continue
        u.groups.remove(group)
        removed.append(u.name)
    timdb.commit()
    return json_response({'removed': removed, 'does_not_belong': does_not_belong, 'not_exist': not_exist})


def get_usernames(usernames):
    usernames = list(set([name.strip() for name in usernames.split(',')]))
    usernames.sort()
    return usernames
