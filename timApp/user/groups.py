from typing import Tuple, List

from flask import Blueprint, abort

from timApp.auth.accesshelper import verify_admin, check_admin_access
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import apply_template, create_document
from timApp.item.validation import ItemValidationRule
from timApp.timdb.dbaccess import get_timdb
from timApp.timdb.sqa import db
from timApp.user.special_group_names import SPECIAL_GROUPS, PRIVILEGED_GROUPS
from timApp.user.user import User, view_access_set, edit_access_set
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.utils import remove_path_special_chars, get_current_time

groups = Blueprint('groups',
                   __name__,
                   url_prefix='/groups')

USER_NOT_FOUND = 'User not found'
USERGROUP_NOT_FOUND = 'User group not found'


def verify_groupadmin():
    if not check_admin_access():
        if not UserGroup.get_groupadmin_group() in get_current_user_object().groups:
            abort(403, 'This action requires group administrator rights.')


def get_uid_gid(groupname, usernames) -> Tuple[UserGroup, List[User]]:
    users = User.query.filter(User.name.in_(usernames)).all()
    group = UserGroup.query.filter_by(name=groupname).first()
    if group is None:
        abort(404, USERGROUP_NOT_FOUND)
    return group, users


@groups.route('/show/<groupname>')
def show_members(groupname):
    ug = UserGroup.get_by_name(groupname)
    if not ug:
        abort(404, USERGROUP_NOT_FOUND)
    verify_group_view_access(ug)
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
    ug = UserGroup.get_by_name(groupname)
    if not ug:
        abort(404, USERGROUP_NOT_FOUND)
    verify_group_view_access(ug)
    u = User.get_by_name(username)
    if not u:
        abort(404, USER_NOT_FOUND)
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
    verify_groupadmin()
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
    u = UserGroup.create(groupname, commit=False)
    doc = create_document(
        f'groups/{remove_path_special_chars(groupname)}',
        groupname,
        validation_rule=ItemValidationRule(check_write_perm=False),
        parent_owner=UserGroup.get_admin_group(),
    )
    apply_template(doc)
    u.admin_doc = doc.block
    f = doc.parent
    if len(f.block.accesses) == 1:
        f.block.accesses.append(BlockAccess(usergroup=UserGroup.get_logged_in_group(),
                                            type=AccessType.view.value,
                                            accessible_from=get_current_time(),
                                            ))
    db.session.commit()
    return json_response(doc)


def verify_group_access(ug: UserGroup, access_set):
    if ug.name in PRIVILEGED_GROUPS:
        verify_admin()
    b = ug.admin_doc
    if not b:
        verify_groupadmin()
    else:
        u = get_current_user_object()
        if not u.has_some_access(b, access_set):
            verify_groupadmin()


def verify_group_edit_access(ug: UserGroup):
    if ug.name in SPECIAL_GROUPS:
        abort(400, 'Cannot edit special groups.')
    verify_group_access(ug, edit_access_set)


def verify_group_view_access(ug: UserGroup):
    verify_group_access(ug, view_access_set)


@groups.route('/addmember/<groupname>/<usernames>')
def add_member(usernames, groupname):
    timdb = get_timdb()
    usernames = get_usernames(usernames)
    group, users = get_uid_gid(groupname, usernames)
    verify_group_edit_access(group)
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
    timdb = get_timdb()
    usernames = get_usernames(usernames)
    group, users = get_uid_gid(groupname, usernames)
    verify_group_edit_access(group)
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
