from dataclasses import field

from timApp.studyinfo.requests import studyinfo_session
from tim_common.marshmallow_dataclass import dataclass


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
    postal_code: str = field(metadata={"data_key": "00100"})
    post_office: str = field(metadata={"data_key": "postitoimipaikka"})
    applications: list[ApplicationItem] = field(metadata={"data_key": "hakukohteet"})


def import_users(applications_oid: str, selection_phase_oid: str) -> None:
    """Import users from a CSV file"""
    with studyinfo_session() as si:
        res = si.get(
            "/valintalaskentakoostepalvelu/resources/hakemukset/valinnanvaihe",
            params={
                "hakuOid": applications_oid,
                "valinnanvaiheOid": selection_phase_oid,
            },
        )
