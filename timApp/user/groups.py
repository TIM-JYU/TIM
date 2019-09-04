from operator import attrgetter
from typing import Tuple, List, Dict, Any

from flask import Blueprint, abort

from timApp.auth.accesshelper import verify_admin, check_admin_access, get_doc_or_abort, verify_view_access
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import apply_template, create_document
from timApp.document.docinfo import DocInfo
from timApp.item.tag import TagType
from timApp.item.validation import ItemValidationRule
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
SISU_PREFIX = 'sisu-'


def verify_groupadmin(require=True, user=None):
    curr_user = user
    if curr_user is None:
        curr_user = get_current_user_object()
    if not check_admin_access(user=user):
        if not UserGroup.get_groupadmin_group() in curr_user.groups:
            if require:
                abort(403, 'This action requires group administrator rights.')
            else:
                return False
    return True


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
    return json_response(sorted(list(ug.users), key=attrgetter('id')))


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
    validate_groupname(groupname)
    u = UserGroup.create(groupname)
    doc = create_document(
        f'groups/{remove_path_special_chars(groupname)}',
        groupname,
        validation_rule=ItemValidationRule(check_write_perm=False),
        parent_owner=UserGroup.get_admin_group(),
    )
    apply_template(doc)
    update_group_doc_settings(doc, groupname)
    add_group_infofield_template(doc)
    u.admin_doc = doc.block
    f = doc.parent
    if len(f.block.accesses) == 1:
        f.block.accesses.append(BlockAccess(usergroup=UserGroup.get_logged_in_group(),
                                            type=AccessType.view.value,
                                            accessible_from=get_current_time(),
                                            ))
    db.session.commit()
    return json_response(doc)


def add_group_infofield_template(doc):
    text = '''
## Omia kenttiä {defaultplugin="textfield" readonly="view"}
{#info autosave: true #}    
    '''
    doc.document.add_text(text)


def update_group_doc_settings(doc: DocInfo, groupname: str, extra_macros: Dict[str, Any]=None):
    s = doc.document.get_settings().get_dict().get('macros', {})
    s['group'] = groupname
    s['fields'] = ['info']
    s['maxRows'] = "40em"  # maxrows for group list
    if extra_macros:
        s.update(extra_macros)
    doc.document.add_setting('macros', s)


def validate_groupname(groupname: str):
    has_digits = False
    has_letters = False
    has_non_alnum = False
    for c in groupname:
        has_digits = has_digits or c.isdigit()
        has_letters = has_letters or c.isalpha()
        has_non_alnum = has_non_alnum or not (c.isalnum() or c.isspace() or c in '-_')
    if not has_digits or not has_letters or has_non_alnum:
        abort(400, 'Usergroup must contain at least one digit and one letter and must not have special chars.')


def verify_group_access(ug: UserGroup, access_set, u=None, require=True):
    if ug.name in PRIVILEGED_GROUPS:
        return verify_admin(require=require, user=u)
    if not u:
        u = get_current_user_object()
    if u.get_personal_group() == ug:
        return True
    b = ug.admin_doc
    if not b:
        return verify_groupadmin(require=require, user=u)
    else:
        if not u.has_some_access(b, access_set):
            return verify_groupadmin(require=require, user=u)
        return True


def verify_group_edit_access(ug: UserGroup, user=None, require=True):
    if ug.name in SPECIAL_GROUPS:
        abort(400, 'Cannot edit special groups.')
    if User.get_by_name(ug.name):
        abort(400, 'Cannot edit personal groups.')
    if ug.name.startswith('cumulative:') or ug.name.startswith('deleted:'):
         abort(400, 'Cannot edit special Sisu groups.')
    verify_group_access(ug, edit_access_set, user, require=require)


def verify_group_view_access(ug: UserGroup, user=None, require=True):
    return verify_group_access(ug, view_access_set, user, require=require)


def get_member_infos(groupname: str, usernames: str):
    usernames = get_usernames(usernames)
    group, users = get_uid_gid(groupname, usernames)
    verify_group_edit_access(group)
    existing_usernames = set(u.name for u in users)
    existing_ids = set(u.id for u in group.users)
    not_exist = [name for name in usernames if name not in existing_usernames]
    return existing_ids, group, not_exist, usernames, users


@groups.route('/addmember/<groupname>/<usernames>')
def add_member(usernames, groupname):
    existing_ids, group, not_exist, usernames, users = get_member_infos(groupname, usernames)
    already_exists = set(u.name for u in group.users) & set(usernames)
    added = []
    curr = get_current_user_object()
    for u in users:
        if u.id not in existing_ids:
            u.add_to_group(group, added_by=curr)
            added.append(u.name)
    db.session.commit()
    return json_response({
        'already_belongs': sorted(list(already_exists)),
        'added': sorted(added),
        'not_exist': sorted(not_exist),
    })


@groups.route('/removemember/<groupname>/<usernames>')
def remove_member(usernames, groupname):
    existing_ids, group, not_exist, usernames, users = get_member_infos(groupname, usernames)
    removed = []
    does_not_belong = []
    ensure_manually_added = group.is_sisu
    su = User.get_scimuser()
    for u in users:
        if u.id not in existing_ids:
            does_not_belong.append(u.name)
            continue
        if ensure_manually_added and group.active_memberships[u.id].adder == su:
            abort(400, 'Cannot remove not-manually-added users from Sisu groups.')
        group.active_memberships[u.id].set_expired()
        removed.append(u.name)
    db.session.commit()
    return json_response({
        'removed': sorted(removed),
        'does_not_belong': sorted(does_not_belong),
        'not_exist': sorted(not_exist),
    })


def is_course(d: DocInfo):
    return any(t.type == TagType.CourseCode for t in d.block.tags)


@groups.route('/enrollToCourse/<int:doc_id>')
def enroll_to_course(doc_id: int):
    d = get_doc_or_abort(doc_id)
    verify_view_access(d)
    if is_course(d):
        course_group = d.document.get_settings().course_group()
        if isinstance(course_group, str):
            ug = UserGroup.get_by_name(course_group)
            if not ug:
                return abort(400, 'The specified course group does not exist')
            verify_group_edit_access(ug, d.owner.users.first())  # TODO assuming only 1 owner which is a person
            ug.users_all.append(get_current_user_object())
            db.session.commit()
            return ok_response()
        else:
            return abort(400, 'Document does not have associated course group')
    else:
        return abort(400, 'Document is not tagged as a course')


def get_usernames(usernames: str):
    usernames = list(set([name.strip() for name in usernames.split(',')]))
    usernames.sort()
    return usernames
