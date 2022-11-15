from dataclasses import field

from timApp.studyinfo.requests import studyinfo_session
from tim_common.marshmallow_dataclass import dataclass, class_schema


@dataclass
class ExamItem:
    id: str = field(metadata={"data_key": "tunniste"})


@dataclass
class ApplicationItem:
    id: str = field(metadata={"data_key": "hakukohdeOid"})
    exams: list[ExamItem] = field(metadata={"data_key": "valintakokeet"})


@dataclass
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
