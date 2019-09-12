import re
from typing import List, Optional, Dict

import attr
from dataclasses import field, dataclass
from flask import Blueprint, request, current_app, Response
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import aliased
from webargs.flaskparser import use_args

from timApp.modules.py.marshmallow_dataclass import class_schema
from timApp.sisu.parse_display_name import parse_sisu_group_display_name
from timApp.sisu.scimusergroup import ScimUserGroup, external_id_re
from timApp.sisu.sisu import refresh_sisu_grouplist_doc, send_course_group_mail
from timApp.tim_app import csrf
from timApp.timdb.sqa import db
from timApp.user.scimentity import get_meta
from timApp.user.user import User, UserOrigin, last_name_to_first, SCIM_USER_NAME
from timApp.user.usergroup import UserGroup, tim_group_to_scim, SISU_GROUP_PREFIX, DELETED_GROUP_PREFIX
from timApp.user.usergroupmember import UserGroupMember, membership_current
from timApp.util.flask.requesthelper import load_data_from_req, JSONException
from timApp.util.flask.responsehelper import json_response
from timApp.util.logger import log_warning
from timApp.util.utils import remove_path_special_chars

scim = Blueprint('scim',
                 __name__,
                 url_prefix='/scim')

UNPROCESSABLE_ENTITY = 422


@dataclass
class SCIMNameModel:
    familyName: str
    givenName: str
    middleName: Optional[str] = None

    def derive_full_name(self, last_name_first: bool):
        if last_name_first:
            full = f'{self.familyName} {self.givenName}'
            if self.middleName:
                full += f' {self.middleName}'
            return full
        else:
            if self.middleName:
                return f'{self.givenName} {self.middleName} {self.familyName}'
            else:
                return f'{self.givenName} {self.familyName}'


@dataclass
class SCIMMemberModel:
    value: str
    name: SCIMNameModel
    display: str
    ref: Optional[str] = field(metadata={'data_key': '$ref'}, default=None)
    type: Optional[str] = None
    email: Optional[str] = None


@dataclass
class SCIMCommonModel:
    externalId: str
    displayName: str


@dataclass
class SCIMEmailModel:
    value: str
    type: Optional[str] = None
    primary: bool = True


@dataclass
class SCIMUserModel(SCIMCommonModel):
    userName: str
    emails: List[SCIMEmailModel]


SCIMUserModelSchema = class_schema(SCIMUserModel)


@dataclass
class SCIMGroupModel(SCIMCommonModel):
    members: List[SCIMMemberModel]
    id: Optional[str] = None
    schemas: Optional[List[str]] = None


SCIMGroupModelSchema = class_schema(SCIMGroupModel)


@attr.s(auto_attribs=True)
class SCIMException(Exception):
    code: int
    msg: str
    headers: Optional[Dict[str, str]] = None


@scim.errorhandler(SCIMException)
def item_locked(error: SCIMException):
    log_warning(error.msg)
    return handle_error_msg_code(error.code, error.msg, error.headers)


def handle_error(error):
    return handle_error_msg_code(error.code, error.description)


def handle_error_msg_code(code: int, msg: str, headers=None):
    return json_response(
        scim_error_json(code, msg),
        status_code=code,
        headers=headers,
    )


scim.errorhandler(UNPROCESSABLE_ENTITY)(handle_error)


def scim_error_json(code, msg):
    return {
        "detail": msg,
        "schemas": ["urn:ietf:params:scim:api:messages:2.0:Error"],
        "status": str(code),
    }


@scim.before_request
def check_auth():
    expected_username = current_app.config.get('SCIM_USERNAME')
    expected_password = current_app.config.get('SCIM_PASSWORD')
    if not expected_username or not expected_password:
        raise SCIMException(403, 'SCIM username or password not configured.')
    headers = {'WWW-Authenticate': 'Basic realm="Authentication required"'}
    auth = request.authorization
    if not auth:
        raise SCIMException(401, 'This action requires authentication.', headers=headers)
    if auth.username == expected_username and auth.password == expected_password:
        pass
    else:
        raise SCIMException(401, 'Incorrect username or password.', headers=headers)


@dataclass
class GetGroupsModel:
    filter: str


GetGroupsModelSchema = class_schema(GetGroupsModel)


def get_scim_id(ug: UserGroup):
    return tim_group_to_scim(ug.name)


filter_re = re.compile('externalId sw (.+)')


def scim_group_to_tim(sisu_group: str):
    return f'{SISU_GROUP_PREFIX}{sisu_group}'


@scim.route('/Groups')
@use_args(GetGroupsModelSchema())
def get_groups(args: GetGroupsModel):
    m = filter_re.fullmatch(args.filter)
    if not m:
        raise SCIMException(422, 'Unsupported filter')
    groups = ScimUserGroup.query.filter(ScimUserGroup.external_id.startswith(scim_group_to_tim(m.group(1)))).join(
        UserGroup).with_entities(UserGroup).all()

    def gen_groups():
        for g in groups:  # type: UserGroup
            yield {
                'id': g.scim_id,
                'externalId': g.scim_id,
                'meta': get_meta(g),
            }

    return json_response({
        'schemas': ['urn:ietf:params:scim:api:messages:2.0:ListResponse'],
        'totalResults': len(groups),
        'Resources': list(gen_groups()),
    })


def derive_scim_group_name(s: str):
    x = parse_sisu_group_display_name(s)
    if not x:
        return remove_path_special_chars(s.lower())
    if x.period:
        return f'{x.coursecode.lower()}-{x.year[2:]}{x.period.lower()}-{x.desc_slug}'
    else:
        return f'{x.coursecode.lower()}-{x.year[2:]}{x.month}{x.day}-{x.desc_slug}'


@csrf.exempt
@scim.route('/Groups', methods=['post'])
@use_args(SCIMGroupModelSchema(), locations=("json",))
def post_group(args: SCIMGroupModel):
    gname = scim_group_to_tim(args.externalId)
    ug = try_get_group_by_scim(args.externalId)
    if ug:
        msg = f'Group already exists: {gname}'
        log_warning(msg)
        log_warning(str(args))
        raise SCIMException(409, msg)
    deleted_group = UserGroup.get_by_name(f'{DELETED_GROUP_PREFIX}{args.externalId}')
    derived_name = derive_scim_group_name(args.displayName)
    if deleted_group:
        ug = deleted_group
        ug.name = derived_name
    else:
        ug = UserGroup(name=derived_name, display_name=args.displayName)
        db.session.add(ug)
    update_users(ug, args)
    db.session.commit()
    return json_response(group_scim(ug), status_code=201)


@scim.route('/Groups/<group_id>')
def get_group(group_id):
    ug = get_group_by_scim(group_id)
    return json_response(group_scim(ug))


@csrf.exempt
@scim.route('/Groups/<group_id>', methods=['put'])
def put_group(group_id: str):
    # log_info(get_request_message(include_body=True))
    try:
        ug = get_group_by_scim(group_id)
        try:
            d = load_data_from_req(SCIMGroupModelSchema)
        except JSONException as e:
            raise SCIMException(422, e.description)
        update_users(ug, d)
        db.session.commit()
        return json_response(group_scim(ug))
    except Exception as e:
        # log_error(traceback.format_exc())
        raise


@csrf.exempt
@scim.route('/Groups/<group_id>', methods=['delete'])
def delete_group(group_id):
    ug = get_group_by_scim(group_id)
    ug.name = f'{DELETED_GROUP_PREFIX}{ug.external_id.external_id}'
    db.session.delete(ug.external_id)
    db.session.commit()
    return Response(status=204)


@scim.route('/Users/<user_id>')
def get_user(user_id):
    u = User.get_by_name(user_id)
    if not u:
        raise SCIMException(404, 'User not found.')
    return json_response(u.get_scim_data())


@csrf.exempt
@scim.route('/Users/<user_id>', methods=['put'])
def put_user(user_id):
    u = User.get_by_name(user_id)
    if not u:
        raise SCIMException(404, 'User not found.')
    try:
        um: SCIMUserModel = load_data_from_req(SCIMUserModelSchema)
    except JSONException as e:
        raise SCIMException(422, e.description)
    u.real_name = last_name_to_first(um.displayName)
    if um.emails:
        u.email = um.emails[0].value
    db.session.commit()
    return json_response(u.get_scim_data())


email_error_re = re.compile(r"Key \(email\)=\((?P<email>[^()]+)\) already exists.")


def update_users(ug: UserGroup, args: SCIMGroupModel):
    external_id = args.externalId
    if not ug.external_id:
        if not external_id_re.fullmatch(external_id):
            raise SCIMException(422, f'Unexpected externalId format: {external_id}')
        ug.external_id = ScimUserGroup(external_id=external_id)
    else:
        if ug.external_id.external_id != args.externalId:
            raise SCIMException(422, 'externalId unexpectedly changed')
    current_usernames = set(u.value for u in args.members)
    removed_user_names = set(u.name for u in ug.users) - current_usernames
    for ms in get_scim_memberships(ug).filter(User.name.in_(removed_user_names)).with_entities(UserGroupMember):
        ms.set_expired()
    p = parse_sisu_group_display_name(args.displayName)
    if not p:
        raise SCIMException(422, f'Unexpected displayName format: {args.displayName}')
    ug.display_name = args.displayName
    emails = [m.email for m in args.members if m.email is not None]
    unique_emails = set(emails)
    if len(emails) != len(unique_emails):
        raise SCIMException(422, f'The users do not have distinct emails.')

    unique_usernames = set(m.value for m in args.members)
    if len(args.members) != len(unique_usernames):
        raise SCIMException(422, f'The users do not have distinct usernames.')

    added_users = set()
    scimuser = User.get_scimuser()
    existing_accounts: List[User] = User.query.filter(User.name.in_(current_usernames) | User.email.in_(emails)).all()
    existing_accounts_dict: Dict[str, User] = {u.name: u for u in existing_accounts}
    existing_accounts_by_email_dict: Dict[str, User] = {u.email: u for u in existing_accounts}
    with db.session.no_autoflush:
        for u in args.members:
            expected_name = u.name.derive_full_name(last_name_first=True)
            consistent = (u.display.endswith(' ' + u.name.familyName)
                          # There are some edge cases that prevent this condition from working, so it has been disabled.
                          # and set(expected_name.split(' ')[1:]) == set(u.display.split(' ')[:-1])
                          )
            if not consistent:
                raise SCIMException(
                    422,
                    f"The display attribute '{u.display}' is inconsistent with the name attributes: "
                    f"given='{u.name.givenName}', middle='{u.name.middleName}', family='{u.name.familyName}'.")
            name_to_use = expected_name
            user = existing_accounts_dict.get(u.value)
            if user:
                if u.email is not None:
                    user_email = existing_accounts_by_email_dict.get(u.email)
                    if user_email and user != user_email:
                        # TODO: Could probably merge users here automatically.
                        raise SCIMException(422, f'Users {user.name} and {user_email.name} must be merged because of conflicting emails.')
                user.update_info(
                    name=u.value,
                    real_name=name_to_use,
                    email=u.email,
                    last_name=u.name.familyName,
                    given_name=u.name.givenName,
                )
            else:
                user = existing_accounts_by_email_dict.get(u.email)
                if user:
                    if not user.is_email_user:
                        raise SCIMException(422, f'Key (email)=({user.email}) already exists. Conflicting username is: {u.value}')
                    user.update_info(
                        name=u.value,
                        real_name=name_to_use,
                        email=u.email,
                        last_name=u.name.familyName,
                        given_name=u.name.givenName,
                    )
                else:
                    user, _ = User.create_with_group(
                        u.value,
                        name_to_use,
                        u.email,
                        origin=UserOrigin.Sisu,
                        last_name=u.name.familyName,
                        given_name=u.name.givenName,
                    )
            added = user.add_to_group(ug, added_by=scimuser)
            if added:
                added_users.add(user)

    try:
        db.session.flush()
    except IntegrityError as e:
        db.session.rollback()
        return raise_conflict_error(args, e)
    refresh_sisu_grouplist_doc(ug)

    # Possibly just checking is_responsible_teacher could be enough.
    if ug.external_id.is_responsible_teacher and not ug.external_id.is_studysubgroup:
        tg = UserGroup.get_teachers_group()
        for u in added_users:
            if tg not in u.groups:
                u.groups.append(tg)
            send_course_group_mail(p, u)


def raise_conflict_error(args, e):
    msg = e.orig.diag.message_detail
    m = email_error_re.fullmatch(msg)
    if m:
        em = m.group('email')
        member = None
        for x in args.members:
            if x.email == em:
                member = x
                break
        msg += " Conflicting username is: " + member.value
    raise SCIMException(422, msg) from e


def is_manually_added(u: User):
    """It is possible to add user manually to SCIM groups.
    For now we assume that any email user is such.
    """
    return u.is_email_user


# Required in group_scim because we need to join User table twice.
user_adder = aliased(User)


def get_scim_memberships(ug: UserGroup):
    return (ug.memberships
            .join(user_adder, UserGroupMember.adder)
            .join(User, UserGroupMember.user)
            .filter(membership_current & (user_adder.name == SCIM_USER_NAME))
            )


def group_scim(ug: UserGroup):
    def members():
        db.session.expire(ug)
        for u in get_scim_memberships(ug).with_entities(User):
            yield {
                'value': u.scim_id,
                '$ref': u.scim_location,
                'display': u.scim_display_name,
            }

    return {
        **ug.get_scim_data(),
        'members': list(members()),
    }


def try_get_group_by_scim(group_id: str):
    try:
        ug = ScimUserGroup.query.filter_by(external_id=scim_group_to_tim(group_id)).join(UserGroup).with_entities(
            UserGroup).first()
    except ValueError:
        raise SCIMException(404, f'Group {group_id} not found')
    return ug


def get_group_by_scim(group_id: str):
    ug = try_get_group_by_scim(group_id)
    if not ug:
        raise SCIMException(404, f'Group {group_id} not found')
    return ug
