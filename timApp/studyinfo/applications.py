import json
from collections import defaultdict
from dataclasses import field, dataclass

from sqlalchemy import func

from timApp.answer.answer import Answer
from timApp.auth.login import create_or_update_user
from timApp.document.docinfo import DocInfo
from timApp.studyinfo.requests import studyinfo_session
from timApp.studyinfo.test_data import TEST_IMPORT_DATA
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo, UserOrigin
from timApp.user.usercontact import ContactOrigin
from tim_common.marshmallow_dataclass import class_schema


@dataclass
class ExamItem:
    id: str = field(metadata={"data_key": "tunniste"})


@dataclass(slots=True)
class ApplicationItem:
    id: str = field(metadata={"data_key": "hakukohdeOid"})
    exams: list[ExamItem] = field(metadata={"data_key": "valintakokeet"})


@dataclass(slots=True)
class StudyInfoApplication:
    id: str = field(metadata={"data_key": "hakemusOid"})
    person_id: str = field(metadata={"data_key": "henkiloOid"})
    first_names: str = field(metadata={"data_key": "etunimet"})
    last_name: str = field(metadata={"data_key": "sukunimi"})
    display_name: str = field(metadata={"data_key": "kutsumanimi"})
    email: str = field(metadata={"data_key": "sahkoposti"})
    address: str = field(metadata={"data_key": "katuosoite"})
    postal_code: str = field(metadata={"data_key": "postinumero"})
    post_office: str = field(metadata={"data_key": "postitoimipaikka"})
    applications: list[ApplicationItem] = field(metadata={"data_key": "hakukohteet"})


StudyInfoApplicationSchema = class_schema(StudyInfoApplication)()


@dataclass
class StudyInfoImportException(Exception):
    applications_oid: str
    selection_phase_oid: str
    message: str

    def __str__(self):
        return (
            f"Could not import users (application: {self.applications_oid}, phase: {self.selection_phase_oid}): "
            f"{self.message}"
        )


def get_student_applications(
    applications_oid: str, selection_phase_oid: str
) -> list[StudyInfoApplication]:
    """Import users from a CSV file"""

    return StudyInfoApplicationSchema.load(json.loads(TEST_IMPORT_DATA), many=True)

    with studyinfo_session() as si:
        res = si.get(
            "/valintalaskentakoostepalvelu/resources/hakemukset/valinnanvaihe",
            params={
                "hakuOid": applications_oid,
                "valinnanvaiheOid": selection_phase_oid,
            },
        )

        if res.status_code == 500:
            raise StudyInfoImportException(
                applications_oid, selection_phase_oid, "Internal API error"
            )
        if res.status_code == 403:
            raise StudyInfoImportException(
                applications_oid, selection_phase_oid, "No permission to import users"
            )

        applications: list[StudyInfoApplication] = StudyInfoApplicationSchema.load(
            res.json(), many=True
        )

        return applications


@dataclass(slots=True)
class ImportRequest:
    applications_oid: str
    selection_phase_oid: str


def import_students(doc: DocInfo, requests: list[ImportRequest]) -> None:
    # Collect all applications together
    applications: list[StudyInfoApplication] = []
    for r in requests:
        applications.extend(
            get_student_applications(r.applications_oid, r.selection_phase_oid)
        )

    # Same person might have multiple applications, so we need to group them by person id
    applications_by_person: dict[str, list[StudyInfoApplication]] = defaultdict(list)
    for a in applications:
        applications_by_person[a.person_id].append(a)

    # Next, find users. For now, use email address since only one person can have a primary email address.
    # Moreover, we don't know user's OID in the first place, so we cannot use that to find the user.
    users_by_email: dict[str, User] = {}
    missing_users: list[UserInfo] = []
    for _, applications in applications_by_person.items():
        a = applications[0]
        user = User.get_by_email(a.email)
        if user is not None:
            users_by_email[a.email] = user
        else:
            missing_users.append(
                UserInfo(
                    username=a.email,
                    email=a.email,
                    full_name=f"{a.last_name} {a.first_names}",
                    given_name=a.display_name,
                    last_name=a.last_name,
                    origin=UserOrigin.StudyInfo,
                )
            )

    # Create missing users -> begins DB transaction
    for user_info in missing_users:
        user = create_or_update_user(user_info, update_email=False)
        user.set_emails(
            [user_info.email], ContactOrigin.StudyInfo, can_update_primary=True
        )
        users_by_email[user_info.email] = user

    db.session.flush()

    # Convert importable data to correct field format
    user_fields: dict[str, list[tuple[str, str]]] = {}

    def as_field(val: str) -> str:
        return json.dumps({"c": val}, ensure_ascii=False)

    for _, applications in applications_by_person.items():
        fields = []
        application_ids = []
        application_item_ids = set()
        exam_item_ids = set()
        for a in applications:
            application_ids.append(a.id)
            for application_item in a.applications:
                application_item_ids.add(application_item.id)
                for exam_item in application_item.exams:
                    exam_item_ids.add(exam_item.id)

            fields.append((f"{doc.id}.personId", as_field(a.person_id)))
            fields.append((f"{doc.id}.address", as_field(a.address)))
            fields.append((f"{doc.id}.postalCode", as_field(a.postal_code)))
            fields.append((f"{doc.id}.postOffice", as_field(a.post_office)))
            fields.append(
                (f"{doc.id}.applicationIds", as_field(",".join(application_ids)))
            )
            fields.append(
                (f"{doc.id}.applicationItems", as_field(",".join(application_item_ids)))
            )
            fields.append((f"{doc.id}.exams", as_field(",".join(exam_item_ids))))

        user_fields[applications[0].email] = fields

    # Next, find all last fields with the given names
    field_names = {name for _, fields in user_fields.items() for name, _ in fields}
    sq = (
        Answer.query.join(User, Answer.users)
        .filter(Answer.task_id.in_(field_names) & Answer.valid.is_(True))
        .group_by(User.id, Answer.task_id)
        .with_entities(func.max(Answer.id).label("aid"), User.email.label("email"))
        .subquery()
    )
    answers: dict[str, dict[str, Answer]] = defaultdict(dict)
    for e, a in Answer.query.join(sq, sq.c.aid == Answer.id).with_entities(
        sq.c.email, Answer
    ):
        answers[e][a.task_id] = a

    # Update or create answers for the fields
    for email, fields in user_fields.items():
        user = users_by_email[email]
        answs = answers[email]
        for name, value in fields:
            if answer := answs.get(name):
                answer.content = value
            else:
                db.session.add(
                    Answer(
                        content=value,
                        task_id=name,
                        users=[user],
                        valid=True,
                    )
                )

    # Finally, commit the changes
    db.session.commit()
