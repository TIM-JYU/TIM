from datetime import date, datetime
from json import JSONDecodeError
from textwrap import dedent
from typing import List, Optional, Dict, Union, Any

import click
import requests
from dataclasses import dataclass, asdict, field
from flask import Blueprint, abort, current_app, request
from flask.cli import AppGroup
from marshmallow import validates, ValidationError
from marshmallow.utils import _Missing, missing
from sqlalchemy import any_, true
from sqlalchemy.exc import IntegrityError
from webargs.flaskparser import use_args

from marshmallow_dataclass import class_schema
from timApp.answer.routes import handle_jsrunner_response
from timApp.auth.accesshelper import get_doc_or_abort, AccessDenied
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
from timApp.tim_app import app, csrf
from timApp.timdb.sqa import db
from timApp.user.groups import validate_groupname, update_group_doc_settings, add_group_infofield_template, \
    verify_group_view_access
from timApp.user.user import User
from timApp.user.usergroup import UserGroup, get_sisu_groups_by_filter
from timApp.util.flask.requesthelper import use_model
from timApp.util.flask.responsehelper import json_response
from timApp.util.get_fields import get_fields_and_users, MembershipFilter
from timApp.util.logger import log_warning
from timApp.util.utils import remove_path_special_chars, seq_to_str, split_location, get_current_time, fin_timezone

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


def get_potential_groups(u: User, course_filter: Optional[str] = None) -> List[UserGroup]:
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


@dataclass
class GroupCreateModel:
    externalId: str
    name: Union[str, _Missing] = missing


GroupCreateSchema = class_schema(GroupCreateModel)


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
        owner_group: UserGroup = None,
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
                """).strip(),
        mail_from=app.config['NOREPLY_EMAIL'],
    )


@dataclass
class PostGradesModel:
    destCourse: str
    docId: int
    dryRun: bool
    partial: bool
    filterUsers: Optional[List[str]] = None
    includeUsers: MembershipFilter = field(default=MembershipFilter.Current, metadata={'by_value': True})
    completionDate: Optional[datetime] = None
    groups: Optional[List[str]] = None


@sisu.route('/sendGrades', methods=['post'])
@use_model(PostGradesModel)
def post_grades_route(m: PostGradesModel):
    result = json_response(send_grades_to_sisu(
        m.destCourse,
        get_current_user_object(),
        get_doc_or_abort(m.docId),
        partial=m.partial,
        dry_run=m.dryRun,
        groups=m.groups,
        filter_users=m.filterUsers,
        completion_date=m.completionDate.astimezone(fin_timezone).date() if m.completionDate else None,
        membership_filter=m.includeUsers,
    ))
    if not m.dryRun:
        db.session.commit()
    return result


class IncorrectSettings(Exception):
    pass


class SisuError(Exception):
    pass


@dataclass
class PostAssessmentsErrorValue:
    code: int
    reason: str


@dataclass
class PostAssessmentsBody:
    assessments: Dict[int, Dict[str, PostAssessmentsErrorValue]]


@dataclass
class PostAssessmentsResponse:
    body: Optional[PostAssessmentsBody] = None
    error: Optional[PostAssessmentsErrorValue] = None


PostAssessmentsResponseSchema = class_schema(PostAssessmentsResponse)


@dataclass
class Assessment:
    userName: str
    gradeId: str
    completionDate: str
    completionCredits: Optional[int] = None
    privateComment: Optional[str] = None

    @validates('gradeId')
    def validate_grade(self, value):
        if value == '':
            raise ValidationError('Cannot be empty')
        if value not in ('0', '1', '2', '3', '4', '5', 'HYV', 'HYL', 'HT', 'TT'):
            raise ValidationError(f'Cannot be "{value}"')
        # if value == 'HYL':
        #     raise ValidationError('Sisu interface currently does not accept HYL grade')


@dataclass
class CandidateAssessment:
    user: User
    gradeId: Any
    completionDate: Any
    completionCredits: Any = None
    privateComment: Any = None
    sentGrade: Any = None

    def to_sisu_json(
            self,
            completion_date: Optional[str] = None,
            ensure_int_credit: bool = False,
    ):
        result = {
            'userName': self.user.name,
            'gradeId': self.gradeId,
            'completionDate': completion_date or self.completionDate,
        }
        if self.completionCredits:
            c = self.completionCredits
            if ensure_int_credit:
                c = int(c)
            result['completionCredits'] = c
        if self.privateComment:
            result['privateComment'] = self.privateComment
        return result

    @property
    def is_fail_grade(self):
        return self.gradeId in ('HYL', '0')

    def is_new_or_changed(self):
        return (not self.sentGrade or self.gradeId != self.sentGrade) and not self.is_fail_grade

    def to_json(self):
        r = asdict(self)
        r.pop('sentGrade')
        return r


AssessmentSchema = class_schema(Assessment)


@csrf.exempt
@sisu.route('/assessments/<sisuid>', methods=['post'])
def mock_assessments(sisuid):
    ok_names = {'us-1'}
    j = request.get_json()
    assessments = j['assessments']
    partial = j['partial']
    return json_response({'body': {
        'assessments': {str(i): {'userName': {'code': 40001, 'reason': 'Some reason.'}}
                        for i, a in enumerate(assessments) if a['userName'] not in ok_names},
    }}, status_code=207 if partial else 400)


def call_sisu_assessments(sisu_id: str, json: Dict[str, Any]):
    url = f'{app.config["SISU_ASSESSMENTS_URL"]}{sisu_id}'
    return requests.post(
        url,
        json=json,
        cert=app.config['SISU_CERT_PATH'],
    )


def send_grades_to_sisu(
        sisu_id: str,
        teacher: User,
        doc: DocInfo,
        partial: bool,
        dry_run: bool,
        completion_date: Optional[date],
        filter_users: Optional[List[str]],
        groups: Optional[List[str]],
        membership_filter: MembershipFilter,
):
    assessments = get_sisu_assessments(sisu_id, teacher, doc, groups, filter_users, membership_filter)
    if not completion_date:
        completion_date = get_current_time().date()
    users_to_update = {a.user.id for a in assessments if a.is_new_or_changed()}
    completion_date_iso = completion_date.isoformat()
    validation_errors = []
    try:
        # noinspection PyTypeChecker
        AssessmentSchema(many=True).load([a.to_sisu_json(completion_date=completion_date_iso) for a in assessments])
    except ValidationError as e:
        validation_errors = [
            {
                'assessment': assessments[i],
                'message': ", ".join(x + ": " + ", ".join(y) for x, y in a.items()),
            }
            for i, a in e.messages.items()
        ]
        if not partial:
            return {
                'sent_assessments': [],
                'default_selection': [],
                'assessment_errors': validation_errors,
            }
        invalid_assessments_indices = {i for i in e.messages.keys()}
        assessments = [a for i, a in enumerate(assessments) if i not in invalid_assessments_indices]
    # log_info(json.dumps(assessments, indent=4))
    r = call_sisu_assessments(
        sisu_id,
        json={
            'assessments': [a.to_sisu_json(
                completion_date=completion_date_iso,
                ensure_int_credit=True,
            ) for a in assessments],
            'partial': partial,
            'dry_run': dry_run,
        },
    )
    # log_info(json.dumps(r.json(), indent=4))
    # noinspection PyTypeChecker
    try:
        pr: PostAssessmentsResponse = PostAssessmentsResponseSchema().load(r.json())
    except JSONDecodeError:
        log_warning(f'Sisu returned invalid JSON: {r.text}')
        raise SisuError('Connection to Sisu is currently not working (Sisu gave an unexpected error).')
    if pr.error:
        raise SisuError(pr.error.reason)
    invalid_assessments = set(n for n in pr.body.assessments.keys())
    ok_assessments = [a for i, a in enumerate(assessments) if i not in invalid_assessments]
    if not dry_run and r.status_code < 400:
        for a in ok_assessments:
            a.completionDate = completion_date_iso
        handle_jsrunner_response(
            {
                'savedata': [
                    {
                        'fields': {
                            f'{doc.id}.completionDate': a.completionDate,
                            f'{doc.id}.sentGrade': a.gradeId,
                        },
                        'user': a.user.id,
                    }
                    for a in ok_assessments
                ],
                'allowMissing': True,
            },
            current_doc=doc,
            allow_non_teacher=False,
        )
        users_to_update = set()
    errs = [
        {
            'message': 'Sisu: ' + ', '.join(x.reason for x in v.values()),
            'assessment': assessments[k],
        }
        for k, v in pr.body.assessments.items()
    ]
    all_errors = errs + validation_errors
    error_users = set(a['assessment'].user.id for a in all_errors)
    return {
        'sent_assessments': ok_assessments if r.status_code < 400 else [],
        'assessment_errors': all_errors,
        'default_selection': sorted(list(users_to_update - error_users)),
    }


def get_sisu_assessments(
        sisu_id: str,
        teacher: User,
        doc: DocInfo,
        groups: Optional[List[str]],
        filter_users: Optional[List[str]],
        membership_filter: MembershipFilter,
) -> List[CandidateAssessment]:
    teachers_group = UserGroup.get_teachers_group()
    if teacher not in teachers_group.users:
        raise AccessDenied('You are not a TIM teacher.')
    pot_groups = get_potential_groups(teacher, course_filter=sisu_id)
    responsible_teachers_group = f'{sisu_id}-responsible-teachers'
    if responsible_teachers_group not in (g.external_id.external_id for g in pot_groups):
        raise AccessDenied(f'You are not a responsible teacher of the course {sisu_id}.')
    if not teacher.has_teacher_access(doc):
        raise AccessDenied('You do not have teacher access to the document.')
    doc_settings = doc.document.get_settings()
    if groups is None:
        try:
            groupsetting = doc_settings.group()
        except ValueError as e:
            raise IncorrectSettings(str(e)) from e
        if not groupsetting:
            raise IncorrectSettings('The document must have "group" setting that indicates the student group name.')
        usergroups = [groupsetting]
    else:
        usergroups = groups
    ugs = UserGroup.query.filter(UserGroup.name.in_(usergroups)).all()
    requested = set(usergroups)
    found = set(ug.name for ug in ugs)
    not_found_gs = requested - found
    if not_found_gs:
        raise IncorrectSettings(f'Usergroup {seq_to_str(sorted(list(not_found_gs)))} not found.')
    for ug in ugs:
        if not verify_group_view_access(ug, require=False):
            raise AccessDenied(f'You do not have access to the group "{ug.name}".')

        # The group doesn't have to be a Sisu group, but if it is, perform a couple of checks.
        if ug.external_id:
            if not ug.external_id.is_student:
                raise IncorrectSettings(f'The group "{ug.name}" is not a Sisu student group.')
            if ug.external_id.course_id != sisu_id:
                raise IncorrectSettings(
                    f'The associated course id "{ug.external_id.course_id}" '
                    f'of the group "{ug.name}" does not match the course setting "{sisu_id}".')
    users, _, _, _ = get_fields_and_users(
        ['grade', 'credit', 'completionDate', 'sentGrade'],
        ugs,
        doc,
        teacher,
        user_filter=User.name.in_(filter_users) if filter_users else None,
        member_filter_type=membership_filter,
    )
    return [fields_to_assessment(r, doc) for r in users]


def fields_to_assessment(r, doc: DocInfo) -> CandidateAssessment:
    fields = r['fields']
    grade = fields.get(f'{doc.id}.grade')
    u = r['user']
    # TODO: Sisu accepts also 'privateComment' field.
    result = CandidateAssessment(
        gradeId=str(grade) if grade is not None else None,
        user=u,
        completionDate=fields.get(f'{doc.id}.completionDate'),
        completionCredits=fields.get(f'{doc.id}.credit'),
        sentGrade=fields.get(f'{doc.id}.sentGrade'),
    )
    return result
