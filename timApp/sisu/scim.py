import json
import re
from typing import List, Optional

import attr
from flask import Blueprint, request, current_app, abort, Response
from marshmallow import Schema, fields, post_load, ValidationError, missing
from webargs.flaskparser import use_args

from timApp.auth.login import create_or_update_user
from timApp.timdb.sqa import db
from timApp.user.scimentity import get_meta
from timApp.user.user import User, UserOrigin
from timApp.user.usergroup import UserGroup, tim_group_to_scim, SISU_GROUP_PREFIX
from timApp.util.flask.responsehelper import json_response
from timApp.util.logger import log_warning

scim = Blueprint('scim',
                 __name__,
                 url_prefix='/scim')

DELETED_GROUP_PREFIX = 'deleted:'
CUMULATIVE_GROUP_PREFIX = 'cumulative:'

UNPROCESSABLE_ENTITY = 422


class SCIMMemberSchema(Schema):
    value = fields.Str(required=True)
    ref = fields.Str(attribute='$ref')
    display = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return SCIMMemberModel(**data)


@attr.s(auto_attribs=True)
class SCIMMemberModel:
    value: str
    display: str
    ref: Optional[str] = missing


class SCIMCommonSchema(Schema):
    externalId = fields.Str(required=True)
    displayName = fields.Str(required=True)


@attr.s(auto_attribs=True)
class SCIMCommonModel:
    externalId: str
    displayName: str


@attr.s(auto_attribs=True)
class SCIMEmailModel:
    value: str
    type: str = missing
    primary: bool = missing


class SCIMEmailSchema(Schema):
    value = fields.Str(required=True)
    type = fields.Str()
    primary = fields.Bool()

    @post_load
    def make_obj(self, data):
        return SCIMEmailModel(**data)


class SCIMUserSchema(SCIMCommonSchema):
    userName = fields.Str(required=True)
    emails = fields.List(fields.Nested(SCIMEmailSchema), required=True)

    @post_load
    def make_obj(self, data):
        return SCIMUserModel(**data)


@attr.s(auto_attribs=True)
class SCIMUserModel(SCIMCommonModel):
    userName: str
    emails: List[SCIMEmailModel]


class SCIMGroupSchema(SCIMCommonSchema):
    members = fields.List(fields.Nested(SCIMMemberSchema), required=True)

    @post_load
    def make_obj(self, data):
        return SCIMGroupModel(**data)


@attr.s(auto_attribs=True)
class SCIMGroupModel(SCIMCommonModel):
    members: List[SCIMMemberModel]


def handle_error(error):
    return json_response(
        scim_error_json(error.code, error.description),
        status_code=error.code,
    )


scim.errorhandler(UNPROCESSABLE_ENTITY)(handle_error)
scim.errorhandler(409)(handle_error)


def scim_error(code: int, msg: str):
    return abort(code, msg)


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
        abort(403, 'SCIM username or password not configured.')
    auth_required = Response(
        'This action requires authentication.',
        401,
        {'WWW-Authenticate': 'Basic realm="Authentication required"'})
    auth = request.authorization
    if not auth:
        return auth_required
    if auth.username == expected_username and auth.password == expected_password:
        pass
    else:
        return Response(
            'Incorrect username or password.',
            401,
            {'WWW-Authenticate': 'Basic realm="Authentication required"'},
        )


class GetGroupsSchema(Schema):
    filter = fields.Str(required=True)

    @post_load
    def post_load(self, data):
        return GetGroupsModel(**data)


@attr.s(auto_attribs=True)
class GetGroupsModel:
    filter: str


def get_scim_id(ug: UserGroup):
    return tim_group_to_scim(ug.name)


filter_re = re.compile('externalId sw (.+)')


def scim_group_to_tim(sisu_group: str):
    return f'{SISU_GROUP_PREFIX}{sisu_group}'


@scim.route('/Groups')
@use_args(GetGroupsSchema())
def get_groups(args: GetGroupsModel):
    m = filter_re.fullmatch(args.filter)
    if not m:
        return scim_error(422, 'Unsupported filter')
    groups = UserGroup.query.filter(UserGroup.name.startswith(scim_group_to_tim(m.group(1)))).all()

    def gen_groups():
        for g in groups:  # type: UserGroup
            yield {
                'id': get_scim_id(g),
                # 'externalId': g.name,
                'meta': get_meta(g),
            }

    return json_response({
        'schemas': ['urn:ietf:params:scim:api:messages:2.0:ListResponse'],
        'totalResults': len(groups),
        'Resources': list(gen_groups()),
    })


@scim.route('/Groups', methods=['post'])
@use_args(SCIMGroupSchema(), locations=("json",))
def post_group(args: SCIMGroupModel):
    gname = scim_group_to_tim(args.externalId)
    ug = UserGroup.get_by_name(gname)
    if ug:
        msg = f'Group already exists: {gname}'
        log_warning(msg)
        log_warning(str(args))
        return scim_error(409, msg)
    deleted_group = UserGroup.get_by_name(f'{DELETED_GROUP_PREFIX}{gname}')
    if deleted_group:
        ug = deleted_group
        ug.name = gname
        ug.display_name = args.displayName
    else:
        ug = UserGroup(name=gname, display_name=args.displayName)
    update_users(ug, args)
    db.session.commit()
    return json_response(group_scim(ug), status_code=201)


@scim.route('/Groups/<group_id>')
def get_group(group_id):
    ug = get_group_by_scim(group_id)
    return json_response(group_scim(ug))


@scim.route('/Groups/<group_id>', methods=['put'])
def put_group(group_id: str):
    ug = get_group_by_scim(group_id)
    d = load_data_from_req(SCIMGroupSchema)
    update_users(ug, d)
    db.session.commit()
    return json_response(group_scim(ug))


@scim.route('/Groups/<group_id>', methods=['delete'])
def delete_group(group_id):
    ug = get_group_by_scim(group_id)
    ug.name = f'{DELETED_GROUP_PREFIX}{ug.name}'
    db.session.commit()
    return Response(status=204)


@scim.route('/Users/<user_id>')
def get_user(user_id):
    u = User.get_by_name(user_id)
    if not u:
        return abort(404, 'User not found.')
    return json_response(u.get_scim_data())


@scim.route('/Users/<user_id>', methods=['put'])
def put_user(user_id):
    u = User.get_by_name(user_id)
    if not u:
        return abort(404, 'User not found.')
    um: SCIMUserModel = load_data_from_req(SCIMUserSchema)
    u.real_name = um.displayName
    if um.emails:
        u.email = um.emails[0].value
    db.session.commit()
    return json_response(u.get_scim_data())


def load_data_from_req(schema):
    ps = schema()
    try:
        j = request.get_json()
        if j is None:
            return scim_error(422, 'JSON payload missing.')
        p = ps.load(j)
    except ValidationError as e:
        return scim_error(422, json.dumps(e.messages, sort_keys=True))
    return p


def update_users(ug: UserGroup, d: SCIMGroupModel):
    removed_user_names = set(u.name for u in ug.users) - set(u.value for u in d.members)
    removed_users = User.query.filter(User.name.in_(removed_user_names)).all()
    for u in removed_users:
        ug.users.remove(u)
    create_sisu_users(d, ug)


def create_sisu_users(args: SCIMGroupModel, ug: UserGroup):
    c_name = f'{CUMULATIVE_GROUP_PREFIX}{ug.name}'
    cumulative_group = UserGroup.get_by_name(c_name)
    if not cumulative_group:
        cumulative_group = UserGroup.create(c_name)
    for u in args.members:
        user = create_or_update_user(
            None,  # email will be given in /Users route
            u.display,
            u.value,
            origin=UserOrigin.Sisu,
            group_to_add=ug,
        )
        if user not in cumulative_group.users:
            cumulative_group.users.append(user)


def group_scim(ug: UserGroup):
    def members():
        for u in ug.users.all():  # type: User
            yield {
                'value': u.scim_id,
                '$ref': u.scim_location,
                'display': u.scim_display_name,
            }

    return {
        **ug.get_scim_data(),
        'members': list(members()),
    }


def get_group_by_scim(group_id: str):
    try:
        ug = UserGroup.get_by_name(scim_group_to_tim(group_id))
    except ValueError:
        return scim_error(404, f'Group {group_id} not found')
    if not ug:
        scim_error(404, f'Group {group_id} not found')
    return ug
