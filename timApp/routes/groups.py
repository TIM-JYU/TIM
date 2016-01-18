from flask import Blueprint, abort

from routes.common import verify_admin, getTimDb, jsonResponse, okJsonResponse
from timdb.users import SPECIAL_GROUPS

groups = Blueprint('groups',
                   __name__,
                   url_prefix='/groups')


def get_uid_gid(groupname, usernames):
    timdb = getTimDb()

    uids = [timdb.users.getUserByName(u) for u in usernames]
    for uid, name in zip(uids, usernames):
        if uid is None:
            abort(404, 'User does not exist: ' + name)
    gid = timdb.users.getUserGroupByName(groupname)
    if gid is None:
        abort(404, 'Usergroup does not exist.')
    return gid, uids


@groups.route('/show/<groupname>')
def show_members(groupname):
    verify_admin()
    timdb = getTimDb()
    if not timdb.users.group_exists(groupname):
        abort(404, 'Usergroup does not exist.')
    members = timdb.users.get_users_for_group(groupname)
    return jsonResponse(members)


@groups.route('/create/<groupname>')
def create_group(groupname):
    """
    Route for creating a usergroup.

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
    timdb = getTimDb()
    if timdb.users.group_exists(groupname):
        abort(400, 'Usergroup already exists.')
    has_digits = False
    has_letters = False
    has_non_alnum = False
    for c in groupname:
        has_digits = has_digits or c.isdigit()
        has_letters = has_letters or c.isalpha()
        has_non_alnum = has_non_alnum or not (c.isalnum() or c.isspace())
    if not has_digits or not has_letters or has_non_alnum:
        abort(400, 'Usergroup must contain at least one digit and one letter and must be alphanumeric.')
    timdb.users.createUserGroup(groupname)
    return okJsonResponse()


@groups.route('/addmember/<groupname>/<usernames>')
def add_member(usernames, groupname):
    verify_admin()
    if groupname in SPECIAL_GROUPS:
        abort(400, 'Cannot add members to special groups.')
    timdb = getTimDb()
    usernames = list(set(usernames.split(',')))
    gid, uids = get_uid_gid(groupname, usernames)
    users = timdb.users.get_users_for_group(groupname)
    already = []
    added = []
    for uid, name in zip(uids, usernames):
        if uid in (u['id'] for u in users):
            already.append(name)
        else:
            timdb.users.addUserToGroup(gid, uid)
            added.append(name)
    return jsonResponse({'already_belongs': already, 'added': added})


@groups.route('/removemember/<groupname>/<usernames>')
def remove_member(usernames, groupname):
    verify_admin()
    if groupname in SPECIAL_GROUPS:
        abort(400, 'Cannot add members to special groups.')
    timdb = getTimDb()
    usernames = list(set(usernames.split(',')))
    gid, uids = get_uid_gid(groupname, usernames)
    removed = []
    does_not_belong = []
    for uid, name in zip(uids, usernames):
        count = timdb.users.remove_membership(uid, gid)
        assert count <= 1
        if count == 0:
            does_not_belong.append(name)
        else:
            removed.append(name)
    return jsonResponse({'removed': removed, 'does_not_belong': does_not_belong})
