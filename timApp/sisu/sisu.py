from dataclasses import dataclass, field
from datetime import date, datetime
from enum import Enum
from json import JSONDecodeError
from textwrap import dedent
from typing import Any, Generator, TypedDict

import requests
from flask import Blueprint, current_app, request, Response
from marshmallow import validates, ValidationError
from marshmallow.utils import _Missing, missing
from sqlalchemy import any_, true, select
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import selectinload
from webargs.flaskparser import use_args

from timApp.auth.accesshelper import get_doc_or_abort, AccessDenied
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import apply_template
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.viewcontext import default_view_ctx
from timApp.item.block import Block, BlockType
from timApp.item.validation import (
    ItemValidationRule,
    validate_item_and_create_intermediate_folders,
    validate_item,
)
from timApp.notification.send_email import send_email
from timApp.plugin.jsrunner.util import FieldSaveUserEntry
from timApp.plugin.plugin import Plugin
from timApp.plugin.pluginexception import PluginException
from timApp.sisu.parse_display_name import (
    SisuDisplayName,
    parse_sisu_group_display_name,
)
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.tim_app import app, csrf
from timApp.timdb.sqa import db, run_sql
from timApp.user.groups import (
    validate_groupname,
    update_group_doc_settings,
    add_group_infofield_template,
    verify_group_view_access,
)
from timApp.user.user import User
from timApp.user.usergroup import (
    UserGroup,
    get_sisu_groups_by_filter,
    get_admin_group_id,
)
from timApp.util.flask.requesthelper import use_model, RouteException
from timApp.util.flask.responsehelper import json_response
from timApp.util.get_fields import (
    get_fields_and_users,
    MembershipFilter,
    UserFieldObj,
    RequestedGroups,
)
from timApp.util.logger import log_warning
from timApp.util.utils import (
    remove_path_special_chars,
    seq_to_str,
    split_location,
    get_current_time,
    fin_timezone,
)
from tim_common.marshmallow_dataclass import class_schema
from tim_common.utils import Missing

sisu = Blueprint("sisu", __name__, url_prefix="/sisu")


@sisu.get("/getPotentialGroups")
def get_potential_groups_route() -> Response:
    u = get_current_user_object()
    result = get_potential_groups(u)

    def get_external_id(g: UserGroup) -> ScimUserGroup:
        assert g.external_id is not None
        return g.external_id

    return json_response(
        [
            {
                "id": g.id,
                "name": g.name,
                "external_id": get_external_id(g).external_id,
                "display_name": g.display_name,
                "doc": g.admin_doc.docentries[0] if g.admin_doc else None,
            }
            for g in result
        ]
    )


# Possible role suffixes, excluding students. Order matters!
role_suffixes = [
    "responsible-teachers",
    "studysubgroup-teachers",
    "teachers",
    "administrative-persons",
]


def get_group_prefix(g: UserGroup) -> str | None:
    """Returns the prefix indicating which Sisu groups the users in this Sisu group shall have access to."""
    eid = g.scim_user_group.external_id
    for s in role_suffixes:
        if eid.endswith(f"-{s}"):
            return eid[: -len(s)] + "%"
    return None


def get_potential_groups(u: User, course_filter: str | None = None) -> list[UserGroup]:
    """Returns all the Sisu groups that the user shall have access to."""
    sisu_group_memberships = (
        u.groups_dyn.join(UserGroup).join(ScimUserGroup).with_entities(UserGroup).all()
    )
    ug_filter: Any = true()
    if not u.is_admin:
        accessible_prefixes = [get_group_prefix(g) for g in sisu_group_memberships]
        ug_filter = ug_filter & ScimUserGroup.external_id.like(
            any_(accessible_prefixes)  # type: ignore
        )
    if course_filter:
        ug_filter = ug_filter & ScimUserGroup.external_id.startswith(
            course_filter + "-"
        )
    gs = get_sisu_groups_by_filter(ug_filter)
    return gs


@dataclass
class GroupCreateModel:
    externalId: str
    name: str | _Missing = missing


GroupCreateSchema = class_schema(GroupCreateModel)


def get_sisu_group_rights(g: UserGroup) -> list[UserGroup]:
    group_names = []
    external_id = g.scim_user_group
    if external_id.is_studysubgroup:
        group_names.append(external_id.without_role + "teachers")
    course_code = external_id.course_id
    for r in role_suffixes:
        group_names.append(course_code + "-" + r)
    return get_sisu_groups_by_filter(ScimUserGroup.external_id.in_(group_names))


@sisu.post("/createGroupDocs")
@use_args(GroupCreateSchema(many=True), locations=("json",))
def create_groups_route(args: list[GroupCreateModel]) -> Response:
    u = get_current_user_object()

    # First, make sure user is eligible for access to all the requested groups.
    allowed_groups = get_potential_groups(u)
    allowed_external_ids = {g.scim_user_group.external_id for g in allowed_groups}
    requested_external_ids = {a.externalId for a in args}
    not_allowed = requested_external_ids - allowed_external_ids
    if not_allowed:
        raise AccessDenied(
            f"You don't have access to all the requested groups: {seq_to_str(sorted(list(not_allowed)))}"
        )

    # Now, create the admin documents for groups that don't yet exist.
    # Rights to already existing documents need to be updated too.
    name_map: dict[str, str | Missing] = {a.externalId: a.name for a in args}
    group_map: dict[str, UserGroup] = {
        g.scim_user_group.external_id: g for g in allowed_groups
    }
    created = []
    updated = []
    admin_id = get_admin_group_id()
    for r in requested_external_ids:
        g = group_map[r]
        name_m = name_map[r]
        if not isinstance(name_m, str):
            name = g.name
        else:
            name = name_m
        if name.strip() == "":
            continue
        validate_groupname(name)
        p = parse_sisu_group_display_name(g.display_name)
        name_no_special = remove_path_special_chars(name)
        if not p:
            raise RouteException(
                f"Failed to parse Sisu group display name: {g.display_name}"
            )
        expected_location = p.group_doc_root
        if g.admin_doc:
            doc: DocInfo = g.admin_doc.docentries[0]
            doc.title = name
            # In theory, the admin doc can have multiple aliases, so we'll only update the one in the official location.
            for d in g.admin_doc.docentries:
                location, short_name = split_location(d.path_without_lang)
                if location != expected_location or short_name == name_no_special:
                    continue
                new_path = f"{location}/{name_no_special}"
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
                f"{expected_location}/{name_no_special}",
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
            raise RouteException(f"The group name '{name}' already exists.")
        update_group_doc_settings(doc, name, extra_macros={"sisugroup": r})
        groups = get_sisu_group_rights(g)
        docblock.add_rights(groups, AccessType.owner)

    db.session.commit()
    return json_response(
        {
            "created": created,
            "updated": updated,
        }
    )


def create_sisu_document(
    item_path: str,
    item_title: str,
    owner_group: UserGroup | None = None,
) -> DocInfo:
    validate_item_and_create_intermediate_folders(
        item_path,
        BlockType.Document,
        owner_group,
        validation_rule=ItemValidationRule(check_write_perm=False, require_login=False),
    )
    return DocEntry.create(item_path, owner_group, item_title)


def refresh_sisu_grouplist_doc(ug: UserGroup) -> None:
    external_id = ug.scim_user_group
    if not external_id.is_student and not external_id.is_studysubgroup:
        gn = parse_sisu_group_display_name(ug.display_name)
        assert gn is not None
        sp = gn.sisugroups_doc_path
        d = DocEntry.find_by_path(sp)
        settings_to_set = {
            "global_plugin_attrs": {
                "all": {
                    "sisugroups": external_id.course_id,
                }
            },
            "macros": {
                "course": gn.coursecode_and_time,
            },
            "preamble": "sisugroups",
        }
        if not d:
            d = create_sisu_document(
                sp, f"Sisu groups for course {gn.coursecode.upper()}", owner_group=ug
            )
            admin_id = get_admin_group_id()
            d.document.modifier_group_id = admin_id
            d.document.set_settings(settings_to_set)
        else:
            d.block.add_rights([ug], AccessType.owner)
            p1 = d.parent
            p2 = p1.parent
            p1.block.add_rights([ug], AccessType.owner)  # type: ignore[union-attr]
            p2.block.add_rights([ug], AccessType.owner)  # type: ignore[union-attr]
            p2.parent.block.add_rights([UserGroup.get_teachers_group()], AccessType.view)  # type: ignore[union-attr]

            # Update rights for already existing activated groups.
            docs = d.parent.get_all_documents(
                query_options=selectinload(DocEntry._block)
                .selectinload(Block.managed_usergroup)
                .selectinload(UserGroup.external_id),
            )
            for doc in docs:
                if doc == d:
                    continue
                group = doc.block.managed_usergroup
                # Do some sanity checks for cases that may theoretically happen if someone manually moves documents
                # in a wrong place.
                if not group:
                    continue
                if not group.external_id:
                    continue
                if group.external_id.course_id != external_id.course_id:
                    continue
                doc.block.add_rights([ug], AccessType.owner)

            s = d.document.get_settings()
            g_attrs = s.global_plugin_attrs()
            has_sisu_attr = False
            valid_settings = False
            if isinstance(g_attrs, dict):
                a = g_attrs.get("all")
                if isinstance(a, dict):
                    sisugroups = a.get("sisugroups")
                    if sisugroups == external_id.course_id:
                        has_sisu_attr = True
                    valid_settings = isinstance(sisugroups, str)
            if has_sisu_attr:
                return
            if not valid_settings:
                d.document.set_settings(settings_to_set)
            else:
                for p in d.document.get_paragraphs():
                    if not p.is_plugin():
                        continue
                    try:
                        plug = Plugin.from_paragraph(p, default_view_ctx)
                    except PluginException:
                        continue
                    if plug.values.get("sisugroups") == external_id.course_id:
                        return
                d.document.modifier_group_id = get_admin_group_id()
                d.document.add_text(
                    f"""
# Sisu groups for course {gn.coursecode_and_time}

``` {{#table_extra plugin="tableForm"}}
sisugroups: {external_id.course_id}
table: true
showInView: true
report: false
maxRows: 40em
realnames: true
buttonText:
autosave: true
cbColumn: true
nrColumn: true
filterRow: true
hide:
  toolbar: 1
  editorButtons: 1
```
                """
                )


def send_course_group_mail(p: SisuDisplayName, u: User) -> None:
    send_email(
        u.email,
        f"Kurssin {p.coursecode} Sisu-ryhmät on kopioitu TIMiin",
        dedent(
            f"""
                Kurssin {p.coursecode} Sisussa olevat ryhmät on kopioitu TIMiin. Ne löytyvät dokumentista:
                
                {current_app.config['TIM_HOST']}/view/{p.sisugroups_doc_path}
                
                Dokumentissa on ohjeet ryhmien käyttämiseen TIMissä.
                
                Tämä viesti tulee kaikille kurssin vastuuopettajille ja hallintohenkilöille.
                """
        ).strip(),
        mail_from=app.config["NOREPLY_EMAIL"],
    )


@dataclass
class PostGradesModel:
    destCourse: str
    docId: int
    dryRun: bool
    partial: bool
    filterUsers: list[str] | None = None
    includeUsers: MembershipFilter = field(
        default=MembershipFilter.All, metadata={"by_value": True}
    )
    completionDate: datetime | None = None
    groups: list[str] | None = None


def verify_sisu_assessments() -> None:
    if not app.config["SISU_ASSESSMENTS_URL"]:
        raise SisuError(app.config["SISU_ASSESSMENTS_DISABLED_MESSAGE"])


@sisu.post("/sendGrades")
@use_model(PostGradesModel)
def post_grades_route(m: PostGradesModel) -> Response:
    verify_sisu_assessments()

    result = json_response(
        send_grades_to_sisu(
            m.destCourse,
            get_current_user_object(),
            get_doc_or_abort(m.docId),
            partial=m.partial,
            dry_run=m.dryRun,
            groups=m.groups,
            filter_users=m.filterUsers,
            completion_date=m.completionDate.astimezone(fin_timezone).date()
            if m.completionDate
            else None,
            membership_filter=m.includeUsers,
        )
    )
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
    # TODO: Temporary workaround to allow floats because Sisu API may sometimes erroneously return these.
    credits: int | float | Missing = missing
    gradeId: str | None = None


AssessmentErrors = dict[str, PostAssessmentsErrorValue]


@dataclass
class PostAssessmentsBody:
    assessments: dict[int, AssessmentErrors]


@dataclass
class PostAssessmentsResponse:
    body: PostAssessmentsBody | None = None
    error: PostAssessmentsErrorValue | None = None


PostAssessmentsResponseSchema = class_schema(PostAssessmentsResponse)


@dataclass
class Assessment:
    userName: str
    gradeId: str
    completionDate: str
    completionCredits: int | None = None
    privateComment: str | None = None

    @validates("gradeId")
    def validate_grade(self, value: str) -> None:
        if value == "":
            raise ValidationError("Cannot be empty")
        if value not in ("0", "1", "2", "3", "4", "5", "HYV", "HYL", "HT", "TT"):
            raise ValidationError(f'Cannot be "{value}"')
        # if value == 'HYL':
        #     raise ValidationError('Sisu interface currently does not accept HYL grade')


def maybe_to_str(s: Any | None) -> str | None:
    if s is None:
        return s
    return str(s)


@dataclass
class CandidateAssessment:
    user: User
    gradeId: Any
    completionDate: Any
    completionCredits: Any = None
    privateComment: Any = None
    sentGrade: Any = None
    sentCredit: Any = None

    def to_sisu_json(
        self,
        completion_date: str | None = None,
        ensure_int_credit: bool = False,
    ) -> dict[str, str]:
        result = {
            "userName": self.user.name,
            "gradeId": self.gradeId,
            "completionDate": completion_date or self.completionDate,
        }
        if self.completionCredits:
            c = self.completionCredits
            if ensure_int_credit:
                c = int(c)
            result["completionCredits"] = c
        if self.privateComment:
            result["privateComment"] = self.privateComment
        return result

    @property
    def is_fail_grade(self) -> bool:
        return self.gradeId in ("HYL", "0")

    @property
    def is_passing_grade(self) -> bool:
        return self.gradeId and not self.is_fail_grade


AssessmentSchema = class_schema(Assessment)


@csrf.exempt
@sisu.post("/assessments/<sisuid>")
def mock_assessments(sisuid: str) -> Response:
    ok_names = {"us-1"}
    j = request.get_json()
    # Part of spec
    assert j is not None and isinstance(j, dict)

    assessments = j["assessments"]
    partial = j["partial"]
    return json_response(
        {
            "body": {
                "assessments": {
                    str(i): {"userName": {"code": 40001, "reason": "Some reason."}}
                    for i, a in enumerate(assessments)
                    if a["userName"] not in ok_names
                },
            }
        },
        status_code=207 if partial else 400,
    )


def call_sisu_assessments(sisu_id: str, json: dict[str, Any]) -> requests.Response:
    verify_sisu_assessments()
    url = f"{app.config['SISU_ASSESSMENTS_URL']}{sisu_id}"
    return requests.post(
        url,
        json=json,
        cert=app.config["SISU_CERT_PATH"],
    )


def get_assessment_fields_to_save(
    doc: DocInfo, c: CandidateAssessment
) -> dict[str, str]:
    result = {
        f"{doc.id}.completionDate": c.completionDate,
        f"{doc.id}.sentGrade": c.gradeId,
    }
    if c.sentCredit is not None:
        result[f"{doc.id}.sentCredit"] = c.sentCredit
    return result


@dataclass
class AssessmentError:
    message: str
    assessment: CandidateAssessment


class DefaultCompletionDate(Enum):
    """
    The default completion date to use if the completion date is not set when sending grades to Sisu.
    """

    Now = 1
    """
    Use the current date as the completion date.
    """
    FromField = 2
    """
    Use the completion date from the document's completionDate field.
    """


@dataclass
class SendGradesResult:
    """
    Result of saved grades.
    """

    sent_assessments: list[CandidateAssessment]
    """
    Assessments that were successfully sent to Sisu.
    """
    assessment_errors: list[AssessmentError]
    """
    Errors that occurred when sending the assessments to Sisu.
    """
    default_selection: list[int]
    """
    The user IDs to mark as successfully sent if the grades were saved successfully.
    """


def send_grades_to_sisu(
    sisu_id: str,
    teacher: User,
    doc: DocInfo,
    partial: bool,
    dry_run: bool,
    completion_date: date | None,
    filter_users: list[str] | None,
    groups: list[str] | None,
    membership_filter: MembershipFilter,
    default_completion_date: DefaultCompletionDate = DefaultCompletionDate.Now,
) -> SendGradesResult:
    """
    Send student grades to Sisu via the S2S Assessments API.

    :param sisu_id: Sisu course implementation ID to send the grades to.
    :param teacher: The teacher sending the grades.
    :param doc: Document that contains the fields with grades and credits.
    :param partial: Whether to send the grades partially (i.e. if some of the grades are invalid, send the valid ones).
    :param dry_run: Whether to perform a dry run (i.e. to just run validation checks on S2S API side).
    :param completion_date: The date when the grades were completed.
    :param filter_users: The users to send the grades for. If None, send for all users.
    :param groups: The groups to send the grades for. If None, the group will be fetched from the document settings.
    :param membership_filter: The membership filter to use when fetching the users.
    :param default_completion_date: The default completion date to use if the completion date is not set in the document.
    :return: Result of saved grades.
    """
    assessments = get_sisu_assessments(
        sisu_id, teacher, doc, groups, filter_users, membership_filter
    )
    if not completion_date:
        completion_date = (
            get_current_time().date()
            if default_completion_date == DefaultCompletionDate.Now
            else None
        )
    users_to_update = {a.user.id for a in assessments if a.is_passing_grade}
    completion_date_iso = completion_date.isoformat() if completion_date else None
    validation_errors = []
    try:
        AssessmentSchema(many=True).load(
            [a.to_sisu_json(completion_date=completion_date_iso) for a in assessments]
        )
    except ValidationError as e:
        msgs = e.messages
        assert isinstance(msgs, dict)

        # If completionCredits has a validation error, msgs will be like:
        # {0: {'completionCredits': [['Invalid value.']]}}
        # so we have to flatten the error list.
        def flatten_error_list(err_list: Any) -> Any:
            if (
                isinstance(err_list, list)
                and len(err_list) > 0
                and isinstance(err_list[0], list)
            ):
                return err_list[0]
            return err_list

        validation_errors = [
            AssessmentError(
                assessment=assessments[i],
                message=", ".join(
                    x + ": " + ", ".join(flatten_error_list(y)) for x, y in a.items()
                ),
            )
            for i, a in msgs.items()
        ]
        if not partial:
            return SendGradesResult(
                sent_assessments=[],
                default_selection=[],
                assessment_errors=validation_errors,
            )
        invalid_assessments_indices = {i for i in msgs.keys()}
        assessments = [
            a for i, a in enumerate(assessments) if i not in invalid_assessments_indices
        ]
    # log_info(json.dumps(assessments, indent=4))
    r = call_sisu_assessments(
        sisu_id,
        json={
            "assessments": [
                a.to_sisu_json(
                    completion_date=completion_date_iso,
                    ensure_int_credit=True,
                )
                for a in assessments
            ],
            "partial": partial,
            "dry_run": dry_run,
        },
    )
    # log_info(json.dumps(r.json(), indent=4))
    try:
        pr: PostAssessmentsResponse = PostAssessmentsResponseSchema().load(r.json())
    except JSONDecodeError:
        log_warning(f"Sisu returned invalid JSON: {r.text}")
        raise SisuError(
            "Connection to Sisu is currently not working (Sisu gave an unexpected error)."
        )
    except ValidationError:
        raise SisuError(f"Failed to validate Sisu JSON: {r.text}")
    if pr.error:
        raise SisuError(pr.error.reason)
    assert pr.body is not None
    invalid_assessments = {n for n in pr.body.assessments.keys()}
    ok_assessments = [
        a for i, a in enumerate(assessments) if i not in invalid_assessments
    ]
    if not dry_run and r.status_code < 400:
        for a in ok_assessments:
            a.completionDate = (
                completion_date_iso
                if default_completion_date == DefaultCompletionDate.Now
                else a.completionDate
            )
            a.sentGrade = a.gradeId
            a.sentCredit = a.completionCredits

        from timApp.plugin.jsrunner.util import save_fields

        save_fields(
            {
                "savedata": [
                    FieldSaveUserEntry(
                        fields=get_assessment_fields_to_save(doc, a),
                        user=a.user.id,
                    )
                    for a in ok_assessments
                ],
                "allowMissing": True,
            },
            teacher,
            current_doc=doc,
            allow_non_teacher=False,
        )
        users_to_update = set()
    errs = [
        AssessmentError(
            message="Sisu: " + ", ".join(list_reasons(v)),
            assessment=assessments[k],
        )
        for k, v in pr.body.assessments.items()
    ]
    all_errors = errs + validation_errors
    error_users = {a.assessment.user.id for a in all_errors}
    return SendGradesResult(
        sent_assessments=ok_assessments if r.status_code < 400 else [],
        assessment_errors=all_errors,
        default_selection=sorted(list(users_to_update - error_users)),
    )


def list_reasons(codes: AssessmentErrors) -> Generator[str, None, None]:
    for k, v in codes.items():
        if v.code == 40009 and v.gradeId and v.credits:
            yield f"{v.reason} ({v.gradeId}, {v.credits} op)"
        else:
            yield v.reason


def get_sisu_assessments(
    sisu_id: str,
    teacher: User,
    doc: DocInfo,
    groups: list[str] | None,
    filter_users: list[str] | None,
    membership_filter: MembershipFilter,
) -> list[CandidateAssessment]:
    teachers_group = UserGroup.get_teachers_group()
    if teacher not in teachers_group.users:
        raise AccessDenied("You are not a TIM teacher.")
    pot_groups = get_potential_groups(teacher, course_filter=sisu_id)
    if not any(
        g.scim_user_group.course_id == sisu_id
        and (
            g.scim_user_group.is_responsible_teacher
            or g.scim_user_group.is_administrative_person
        )
        for g in pot_groups
    ):
        raise AccessDenied(
            f"You are neither a responsible teacher nor an administrative person of the course {sisu_id}."
        )
    if not teacher.has_teacher_access(doc):
        raise AccessDenied("You do not have teacher access to the document.")
    doc_settings = doc.document.get_settings()
    if groups is None:
        try:
            groups_setting = doc_settings.groups()
        except ValueError as e:
            raise IncorrectSettings(str(e)) from e
        if not groups_setting:
            raise IncorrectSettings(
                'The document must have "group" setting that indicates the student group name.'
            )
        usergroups = groups_setting
    else:
        usergroups = groups
    ugs = list(
        run_sql(select(UserGroup).filter(UserGroup.name.in_(usergroups)))
        .scalars()
        .all()
    )
    requested = set(usergroups)
    found = {ug.name for ug in ugs}
    not_found_gs = requested - found
    if not_found_gs:
        raise IncorrectSettings(
            f"Usergroup {seq_to_str(sorted(list(not_found_gs)))} not found."
        )
    for ug in ugs:
        if not verify_group_view_access(ug, require=False):
            raise AccessDenied(f'You do not have access to the group "{ug.name}".')

        # The group doesn't have to be a Sisu group, but if it is, perform a couple of checks.
        if ug.external_id:
            if not ug.external_id.is_student:
                raise IncorrectSettings(
                    f'The group "{ug.name}" is not a Sisu student group.'
                )
            if ug.external_id.course_id != sisu_id:
                raise IncorrectSettings(
                    f'The associated course id "{ug.external_id.course_id}" '
                    f'of the group "{ug.name}" does not match the course setting "{sisu_id}".'
                )
    users, _, _, _ = get_fields_and_users(
        ["grade", "credit", "completionDate", "sentGrade", "sentCredit"],
        RequestedGroups(ugs),
        doc,
        teacher,
        default_view_ctx,
        user_filter=User.name.in_(filter_users) if filter_users else None,
        member_filter_type=membership_filter,
    )
    return [fields_to_assessment(r, doc) for r in users]


def fields_to_assessment(r: UserFieldObj, doc: DocInfo) -> CandidateAssessment:
    fields = r["fields"]
    grade = fields.get(f"{doc.id}.grade")
    u = r["user"]
    # TODO: Sisu accepts also 'privateComment' field.
    result = CandidateAssessment(
        gradeId=str(grade) if grade is not None else None,
        user=u,
        completionDate=fields.get(f"{doc.id}.completionDate"),
        completionCredits=fields.get(f"{doc.id}.credit"),
        sentGrade=fields.get(f"{doc.id}.sentGrade"),
        sentCredit=fields.get(f"{doc.id}.sentCredit"),
    )
    return result
