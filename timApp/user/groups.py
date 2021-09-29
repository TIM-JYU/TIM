from dataclasses import dataclass
from operator import attrgetter
from typing import Any, Optional

from flask import Blueprint

from timApp.auth.accesshelper import verify_admin, check_admin_access, AccessDenied
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import apply_template, create_document
from timApp.document.docinfo import DocInfo
from timApp.item.validation import ItemValidationRule
from timApp.timdb.sqa import db
from timApp.user.special_group_names import SPECIAL_GROUPS, PRIVILEGED_GROUPS, SPECIAL_USERNAMES
from timApp.user.user import User, view_access_set, edit_access_set
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import load_data_from_req, RouteException, NotExist
from timApp.util.flask.responsehelper import json_response
from timApp.util.utils import remove_path_special_chars, get_current_time
from tim_common.marshmallow_dataclass import class_schema

groups = Blueprint('groups',
                   __name__,
                   url_prefix='/groups')

USER_NOT_FOUND = 'User not found'


def verify_groupadmin(
        require: bool = True,
        user: Optional[User] = None,
        action: Optional[str] = None,
        msg: Optional[str] = None,
):
    curr_user = user
    if curr_user is None:
        curr_user = get_current_user_object()
    if not check_admin_access(user=user):
        if not UserGroup.get_groupadmin_group() in curr_user.groups:
            if require:
                msg = msg or 'This action requires group administrator rights.'
                if action:
                    msg = action + ': ' + msg
                raise AccessDenied(msg)
            else:
                return False
    return True


def get_uid_gid(groupname, usernames) -> tuple[UserGroup, list[User]]:
    users = User.query.filter(User.name.in_(usernames)).all()
    group = UserGroup.query.filter_by(name=groupname).first()
    raise_group_not_found_if_none(groupname, group)
    return group, users


@groups.get('/getOrgs')
def get_organizations():
    return json_response(UserGroup.get_organizations())


@groups.get('/show/<groupname>')
def show_members(groupname):
    ug = get_group_or_abort(groupname)
    verify_group_view_access(ug)
    return json_response(sorted(list(ug.users), key=attrgetter('id')))


@groups.get('/usergroups/<username>')
def show_usergroups(username):
    verify_admin()
    u = User.get_by_name(username)
    if not u:
        raise NotExist(USER_NOT_FOUND)
    return json_response(u.get_groups(include_special=False).order_by(UserGroup.name).all())


@groups.get('/belongs/<username>/<groupname>')
def belongs(username, groupname):
    ug = get_group_or_abort(groupname)
    verify_group_view_access(ug)
    u = User.get_by_name(username)
    if not u:
        raise NotExist(USER_NOT_FOUND)
    return json_response({'status': ug in u.groups})


def get_group_or_abort(groupname: str):
    ug = UserGroup.get_by_name(groupname)
    raise_group_not_found_if_none(groupname, ug)
    return ug


def raise_group_not_found_if_none(groupname: str, ug: Optional[UserGroup]):
    if not ug:
        raise RouteException(f'User group "{groupname}" not found')


@groups.get('/create/<groupname>')
def create_group(groupname: str):
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
    verify_groupadmin(action=f'Creating group {groupname}')
    if UserGroup.get_by_name(groupname):
        raise RouteException('User group already exists.')
    _, doc = do_create_group(groupname)
    db.session.commit()
    return json_response(doc)


def do_create_group(groupname: str) -> tuple[UserGroup, DocInfo]:
    verify_groupadmin(action=f'Creating group {groupname}')
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
        logged_group = UserGroup.get_logged_in_group()
        f.block.accesses[(logged_group.id, AccessType.view.value)] = (
            BlockAccess(
                usergroup_id=logged_group.id,
                type=AccessType.view.value,
                accessible_from=get_current_time(),
            ))
    return u, doc


def add_group_infofield_template(doc: DocInfo) -> None:
    text = '''
## Omia kenttiä {defaultplugin="textfield" readonly="view" .hidden-print}
{#info autosave: true #}    
    '''
    doc.document.add_text(text)


def update_group_doc_settings(doc: DocInfo, groupname: str, extra_macros: dict[str, Any] = None):
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
        raise RouteException(
            'Usergroup must contain at least one digit and one letter and must not have special chars: "' +
            groupname + '"')


def verify_group_access(ug: UserGroup, access_set, u=None, require=True):
    if ug.name in PRIVILEGED_GROUPS:
        return verify_admin(require=require, user=u)
    if not u:
        u = get_current_user_object()
    if u.get_personal_group() == ug:
        return True
    b = ug.admin_doc
    no_access_msg = f'No access for group {ug.name}'
    if not b:
        return verify_groupadmin(require=require, user=u, msg=no_access_msg)
    else:
        if not u.has_some_access(b, access_set):
            return verify_groupadmin(require=require, user=u, msg=no_access_msg)
        return True


def verify_group_edit_access(ug: UserGroup, user: Optional[User] = None, require=True):
    if ug.name in SPECIAL_GROUPS:
        raise RouteException(f'Cannot edit special group: {ug.name}')
    if User.get_by_name(ug.name):
        raise RouteException(f'Cannot edit personal group: {ug.name}')
    if ug.name.startswith('cumulative:') or ug.name.startswith('deleted:'):
        raise RouteException(f'Cannot edit special Sisu group: {ug.name}')
    return verify_group_access(ug, edit_access_set, user, require=require)


def verify_group_view_access(ug: UserGroup, user=None, require=True):
    return verify_group_access(ug, view_access_set, user, require=require)


def get_member_infos(groupname: str, usernames: list[str]):
    usernames = get_usernames(usernames)
    group, users = get_uid_gid(groupname, usernames)
    verify_group_edit_access(group)
    existing_usernames = {u.name for u in users}
    existing_ids = {u.id for u in group.users}
    not_exist = [name for name in usernames if name not in existing_usernames]
    return existing_ids, group, not_exist, usernames, users


@dataclass
class NamesModel:
    names: list[str]


NamesModelSchema = class_schema(NamesModel)


@groups.post('/addmember/<groupname>')
def add_member(groupname):
    nm: NamesModel = load_data_from_req(NamesModelSchema)
    existing_ids, group, not_exist, usernames, users = get_member_infos(groupname, nm.names)
    if set(nm.names) & SPECIAL_USERNAMES:
        raise RouteException('Cannot add special users.')
    already_exists = {u.name for u in group.users} & set(usernames)
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


@groups.post('/removemember/<groupname>')
def remove_member(groupname):
    nm: NamesModel = load_data_from_req(NamesModelSchema)
    existing_ids, group, not_exist, usernames, users = get_member_infos(groupname, nm.names)
    removed = []
    does_not_belong = []
    ensure_manually_added = group.is_sisu
    su = User.get_scimuser()
    for u in users:
        if u.id not in existing_ids:
            does_not_belong.append(u.name)
            continue
        if ensure_manually_added and group.current_memberships[u.id].adder == su:
            raise RouteException('Cannot remove not-manually-added users from Sisu groups.')
        group.current_memberships[u.id].set_expired()
        removed.append(u.name)
    db.session.commit()
    return json_response({
        'removed': sorted(removed),
        'does_not_belong': sorted(does_not_belong),
        'not_exist': sorted(not_exist),
    })


def get_usernames(usernames: list[str]):
    usernames = list({name.strip() for name in usernames})
    usernames.sort()
    return usernames
