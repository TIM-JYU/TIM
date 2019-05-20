import json
import re
from datetime import datetime
from typing import List, Optional

import attr
from flask import Blueprint, request, current_app, abort, Response
from marshmallow import Schema, fields, post_load, ValidationError
from webargs.flaskparser import use_args

from timApp.auth.login import create_or_update_user
from timApp.timdb.sqa import db
from timApp.user.user import User, UserOrigin
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import json_response
from timApp.util.logger import log_warning

scim = Blueprint('scim',
                 __name__,
                 url_prefix='/scim')

SISU_GROUP_PREFIX = 'sisu:'
DELETED_GROUP_PREFIX = 'deleted:'
CUMULATIVE_GROUP_PREFIX = 'cumulative:'

DEFAULT_TIMESTAMP = datetime(
    year=2015,
    month=1,
    day=1,
)

UNPROCESSABLE_ENTITY = 422


class SCIMMemberSchema(Schema):
    value = fields.Str(required=True)
    ref = fields.Str(attribute='$ref')
    display = fields.Str(required=True)

    @post_load
    def make_obj(self, data):
        return SCIMMemberModel(**data)

    class Meta:
        strict = True


class SCIMGroupSchema(Schema):
    externalId = fields.Str(required=True)
    displayName = fields.Str(required=True)
    members = fields.List(fields.Nested(SCIMMemberSchema), required=True)

    @post_load
    def make_obj(self, data):
        return SCIMGroupModel(**data)

    class Meta:
        strict = True


@attr.s(auto_attribs=True)
class SCIMMemberModel:
    value: str
    display: str
    ref: Optional[str] = None


@attr.s(auto_attribs=True)
class SCIMGroupModel:
    externalId: str
    displayName: str
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


GROUP_ID_PREFIX = 'group-'


def get_scim_id(ug: UserGroup):
    return f'{GROUP_ID_PREFIX}{ug.id}'


def get_tim_group_id(scim_id: str):
    return int(scim_id.replace(GROUP_ID_PREFIX, ''))


filter_re = re.compile('externalId sw (.+)')


@scim.route('/Groups')
@use_args(GetGroupsSchema(strict=True))
def get_groups(args: GetGroupsModel):
    m = filter_re.fullmatch(args.filter)
    if not m:
        return scim_error(422, 'Unsupported filter')
    groups = UserGroup.query.filter(UserGroup.name.startswith(m.group(1))).all()

    def gen_groups():
        for g in groups:  # type: UserGroup
            yield {
                'id': get_scim_id(g),
                # 'externalId': g.name,
                'meta': get_group_meta(g),
            }

    return json_response({
        'schemas': ['urn:ietf:params:scim:api:messages:2.0:ListResponse'],
        'totalResults': len(groups),
        'Resources': list(gen_groups()),
    })


@scim.route('/Groups', methods=['post'])
@use_args(SCIMGroupSchema(strict=True), locations=("json",))
def post_group(args: SCIMGroupModel):
    gname = args.externalId
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
    ps = SCIMGroupSchema()
    try:
        j = request.get_json()
        if j is None:
            return scim_error(422, 'JSON payload missing.')
        p = ps.load(j)
    except ValidationError as e:
        return scim_error(422, json.dumps(e.messages, sort_keys=True))
    d: SCIMGroupModel = p.data
    update_users(ug, d)
    db.session.commit()
    return json_response(group_scim(ug))


def update_users(ug: UserGroup, d: SCIMGroupModel):
    removed_user_names = set(u.name for u in ug.users) - set(u.value for u in d.members)
    removed_users = User.query.filter(User.name.in_(removed_user_names)).all()
    for u in removed_users:
        ug.users.remove(u)
    create_sisu_users(d, ug)


@scim.route('/Groups/<group_id>', methods=['delete'])
def delete_group(group_id):
    ug = get_group_by_scim(group_id)
    ug.name = f'{DELETED_GROUP_PREFIX}{ug.name}'
    db.session.commit()
    return Response(status=204)


def create_sisu_users(args: SCIMGroupModel, ug: UserGroup):
    c_name = f'{CUMULATIVE_GROUP_PREFIX}{ug.name}'
    cumulative_group = UserGroup.get_by_name(c_name)
    if not cumulative_group:
        cumulative_group = UserGroup.create(c_name)
    for u in args.members:
        user = create_or_update_user(
            f'{u.value}@jyu.fi',  # TODO will SCIM give email?
            u.display,
            u.value,
            origin=UserOrigin.Sisu,
            group_to_add=ug,
        )
        if user not in cumulative_group.users:
            cumulative_group.users.append(user)


def get_group_meta(g: UserGroup):
    host = current_app.config['TIM_HOST']
    return {
        'created': g.created or DEFAULT_TIMESTAMP,
        'lastModified': g.modified or DEFAULT_TIMESTAMP,
        'location': f'{host}/scim/Groups/{g.name}',
        'resourceType': 'Group',
        # 'version': '',
    }


def group_scim(ug: UserGroup):
    host = current_app.config['TIM_HOST']

    def members():
        for u in ug.users.all():  # type: User
            yield {
                'value': u.name,
                '$ref': f'{host}/scim/Users/{u.id}',  # This route does not exist, but Sisu won't use it.
                'display': u.real_name,
            }

    return {
        'schemas': ["urn:ietf:params:scim:schemas:core:2.0:Group"],
        'id': get_scim_id(ug),
        # 'externalId': ug.name,
        'meta': get_group_meta(ug),
        'displayName': ug.display_name,
        'members': list(members()),
    }


def get_group_by_scim(group_id: str):
    if not group_id.startswith(GROUP_ID_PREFIX):
        scim_error(404, f'Group {group_id} not found')
    try:
        ug = UserGroup.query.get(get_tim_group_id(group_id))
    except ValueError:
        return scim_error(404, f'Group {group_id} not found')
    if not ug:
        scim_error(404, f'Group {group_id} not found')
    if ug.name.startswith(DELETED_GROUP_PREFIX):
        scim_error(404, f'Group {group_id} not found')
    return ug
