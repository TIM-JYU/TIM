from textwrap import dedent
from typing import List, Optional, Dict

import attr
import click
from flask import Blueprint, abort, current_app
from flask.cli import AppGroup
from marshmallow import Schema, fields, post_load, pre_load
from sqlalchemy import any_, true
from sqlalchemy.exc import IntegrityError
from webargs.flaskparser import use_args

from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import apply_template
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.item.block import Block, BlockType
from timApp.item.validation import ItemValidationRule, validate_item_and_create_intermediate_folders, validate_item
from timApp.notification.notify import send_email
from timApp.sisu.parse_display_name import SisuDisplayName, parse_sisu_group_display_name
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.groups import validate_groupname, update_group_doc_settings, add_group_infofield_template
from timApp.user.user import User
from timApp.user.usergroup import UserGroup, get_sisu_groups_by_filter
from timApp.util.flask.responsehelper import json_response
from timApp.util.utils import remove_path_special_chars, seq_to_str, split_location

sisu = Blueprint('sisu',
                 __name__,
                 url_prefix='/sisu')


@sisu.route('/getPotentialGroups')
def get_potential_groups_route():
    u = get_current_user_object()
    result = get_potential_groups(u)
    return json_response([
        {
            'id': g.id,
            'name': g.name,
            'external_id': g.external_id.external_id,
            'display_name': g.display_name,
            'doc': g.admin_doc.docentries[0] if g.admin_doc else None,
        } for g in result
    ])


# Possible role suffixes, excluding students. Order matters!
role_suffixes = [
    'responsible-teachers',
    'studysubgroup-teachers',
    'teachers',
    'administrative-persons',
]


def get_group_prefix(g: UserGroup):
    """Returns the prefix indicating which Sisu groups the users in this Sisu group shall have access to.
    """
    eid = g.external_id.external_id
    for s in role_suffixes:
        if eid.endswith(f'-{s}'):
            return eid[:-len(s)] + '%'
    return None


def get_potential_groups(u: User, course_filter: str=None) -> List[UserGroup]:
    """Returns all the Sisu groups that the user shall have access to."""
    sisu_group_memberships = u.groups_dyn.join(UserGroup).join(ScimUserGroup).with_entities(UserGroup).all()
    ug_filter = true()
    if not u.is_admin:
        accessible_prefixes = [get_group_prefix(g) for g in sisu_group_memberships]
        ug_filter = ug_filter & ScimUserGroup.external_id.like(any_(accessible_prefixes))
    if course_filter:
        ug_filter = ug_filter & ScimUserGroup.external_id.startswith(course_filter + '-')
    gs = get_sisu_groups_by_filter(ug_filter)
    return gs


class GroupCreateSchema(Schema):
    externalId = fields.Str(required=True)
    name = fields.Str()

    @pre_load
    def preload(self, data):
        if not isinstance(data, dict):
            return data
        ref = data.pop('$ref', None)
        if ref:
            data['ref'] = ref
        return data

    @post_load
    def make_obj(self, data):
        return GroupCreateModel(**data)


@attr.s(auto_attribs=True)
class GroupCreateModel:
    externalId: str
    name: Optional[str] = None


def get_sisu_group_rights(g: UserGroup) -> List[UserGroup]:
    group_names = []
    if g.external_id.is_studysubgroup:
        group_names.append(g.external_id.without_role + 'teachers')
    course_code = g.external_id.course_id
    for r in role_suffixes:
        group_names.append(course_code + '-' + r)
    return get_sisu_groups_by_filter(ScimUserGroup.external_id.in_(group_names))


@sisu.route('/createGroupDocs', methods=['post'])
@use_args(GroupCreateSchema(many=True), locations=("json",))
def create_groups_route(args: List[GroupCreateModel]):
    u = get_current_user_object()

    # First, make sure user is eligible for access to all the requested groups.
    allowed_groups = get_potential_groups(u)
    allowed_external_ids = set(g.external_id.external_id for g in allowed_groups)
    requested_external_ids = set(a.externalId for a in args)
    not_allowed = requested_external_ids - allowed_external_ids
    if not_allowed:
        return abort(403, f"You don't have access to all the requested groups: {seq_to_str(sorted(list(not_allowed)))}")

    # Now, create the admin documents for groups that don't yet exist.
    # Rights to already existing documents need to be updated too.
    name_map: Dict[str, Optional[str]] = {a.externalId: a.name for a in args}
    group_map: Dict[str, UserGroup] = {g.external_id.external_id: g for g in allowed_groups}
    created = []
    updated = []
    admin_id = UserGroup.get_admin_group().id
    for r in requested_external_ids:
        g = group_map[r]
        name = name_map[r]
        if not name:
            name = g.name
        if name.strip() == "":
            continue
        validate_groupname(name)
        p = parse_sisu_group_display_name(g.display_name)
        name_no_special = remove_path_special_chars(name)
        if not p:
            return abort(400, f'Failed to parse Sisu group display name: {g.display_name}')
        expected_location = p.group_doc_root
        if g.admin_doc:
            doc = g.admin_doc.docentries[0]
            doc.title = name
            # In theory, the admin doc can have multiple aliases, so we'll only update the one in the official location.
            for d in g.admin_doc.docentries:
                location, short_name = split_location(d.path_without_lang)
                if location != expected_location or short_name == name_no_special:
                    continue
                new_path = f'{location}/{name_no_special}'
                validate_item(
                    new_path,
                    BlockType.Document,
                    ItemValidationRule(check_write_perm=False, require_login=False),
                )
                d.name = new_path
                updated.append(d)
            doc.document.modifier_group_id = admin_id
        else:
            doc = create_sisu_document(
                f'{expected_location}/{name_no_special}',
                name,
                owner_group=None,
            )
            doc.document.modifier_group_id = admin_id
            apply_template(doc)
            add_group_infofield_template(doc)
            g.admin_doc = doc.block
            created.append(doc)
        docblock: Block = g.admin_doc
        g.name = name
        try:
            db.session.flush()
        except IntegrityError:
            db.session.rollback()
            return abort(400, f"The group name '{name}' already exists.")
        update_group_doc_settings(doc, name, extra_macros={'sisugroup': r})
        groups = get_sisu_group_rights(g)
        docblock.add_rights(groups, AccessType.owner)

    db.session.commit()
    return json_response({
        'created': created,
        'updated': updated,
    })


def create_sisu_document(
        item_path: str,
        item_title: str,
        owner_group: UserGroup=None,
) -> DocInfo:
    validate_item_and_create_intermediate_folders(
        item_path,
        BlockType.Document,
        owner_group,
        validation_rule=ItemValidationRule(check_write_perm=False, require_login=False),
    )
    return DocEntry.create(item_path, owner_group, item_title)


sisu_cli = AppGroup('sisu')

@sisu_cli.command('createdocs')
def create_docs():
    all_sisu_groups = get_sisu_groups_by_filter(true())
    for g in all_sisu_groups:
        print(f'Refreshing {g.external_id.external_id}')
        refresh_sisu_grouplist_doc(g)
    db.session.commit()


@sisu_cli.command('sendmail')
@click.argument('courses', nargs=-1)
def send_course_mail_cli(courses: List[str]):
    for course in courses:
        ug = UserGroup.get_by_external_id(f'{course}-responsible-teachers')
        if not ug:
            print(f'Could not find the responsible teachers group for course {course}. '
                  'Make sure you typed the course in format "jy-CUR-xxxx".')
            return
        p = parse_sisu_group_display_name(ug.display_name)
        for u in ug.users:
            print(f'Sending mail to {u.real_name} {u.email}')
            send_course_group_mail(p, u)


app.cli.add_command(sisu_cli)


def refresh_sisu_grouplist_doc(ug: UserGroup):
    if not ug.external_id.is_student and not ug.external_id.is_studysubgroup:
        gn = parse_sisu_group_display_name(ug.display_name)
        p = gn.sisugroups_doc_path
        d = DocEntry.find_by_path(p)
        if not d:
            d = create_sisu_document(p, f'Sisu groups for course {gn.coursecode.upper()}', owner_group=ug)
            admin_id = UserGroup.get_admin_group().id
            d.document.modifier_group_id = admin_id
            d.document.set_settings({
                'global_plugin_attrs': {
                    'all': {
                        'sisugroups': ug.external_id.course_id,
                    }
                },
                'macros': {
                    'course': gn.coursecode_and_time,
                },
                'preamble': 'sisugroups',
            })
        else:
            d.block.add_rights([ug], AccessType.owner)


def send_course_group_mail(p: SisuDisplayName, u: User):
    send_email(
        u.email,
        f'Kurssin {p.coursecode} Sisu-ryhmät on kopioitu TIMiin',
        dedent(
            f"""
                Kurssin {p.coursecode} Sisussa olevat ryhmät on kopioitu TIMiin. Ne löytyvät dokumentista:
                
                {current_app.config['TIM_HOST']}/view/{p.sisugroups_doc_path}
                
                Dokumentissa on ohjeet ryhmien käyttämiseen TIMissä.
                
                Tämä viesti tulee kaikille kurssin vastuuopettajille.
                """).strip()
    )
