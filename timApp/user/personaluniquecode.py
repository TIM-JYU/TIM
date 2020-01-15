from typing import Optional

from dataclasses import dataclass

from timApp.timdb.sqa import db
from timApp.user.hakaorganization import HakaOrganization


class PersonalUniqueCode(db.Model):
    """The database model for the 'schacPersonalUniqueCode' Haka attribute."""

    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False, primary_key=True)
    """User id."""

    org_id = db.Column(db.Integer, db.ForeignKey('haka_organization.id'), nullable=False, primary_key=True)
    """Organization id."""

    code = db.Column(db.Text, nullable=False, index=True)
    """The actual code. This could be e.g. student id or employee id."""

    type = db.Column(db.Text, nullable=False, primary_key=True)
    """The type of the code, e.g. student or employee."""

    user = db.relationship('User', back_populates='uniquecodes', lazy='joined')
    organization = db.relationship('HakaOrganization', back_populates='uniquecodes', lazy='joined')

    __table_args__ = (db.UniqueConstraint('org_id', 'code', 'type'),)

    @property
    def user_collection_key(self):
        return self.org_id, self.type

    @staticmethod
    def find_by_student_id(sid: str, org: str) -> Optional['PersonalUniqueCode']:
        return PersonalUniqueCode.find_by_code(sid, org, 'studentID')

    @staticmethod
    def find_by_code(code: str, org: str, codetype: str) -> Optional['PersonalUniqueCode']:
        return (PersonalUniqueCode
                .query
                .filter_by(code=code, type=codetype)
                .join(HakaOrganization)
                .filter_by(name=org).first())

    @staticmethod
    def find_by_urn(urn: str) -> Optional['PersonalUniqueCode']:
        p = SchacPersonalUniqueCode.parse(urn)
        if not p:
            return None
        return PersonalUniqueCode.find_by_code(p.code, p.org, p.codetype)


uc_start = 'urn:schac:personalUniqueCode:int:'


@dataclass
class SchacPersonalUniqueCode:
    """Represents the 'schacPersonalUniqueCode' Haka attribute."""

    code: str
    codetype: str
    org: str

    @staticmethod
    def parse(urn: str):
        if not urn.startswith(uc_start):
            return None
        parts = urn.split(':')
        if len(parts) != 7:
            return None
        codetype = parts[-3]
        org_name = parts[-2]
        code = parts[-1]
        return SchacPersonalUniqueCode(code=code, codetype=codetype, org=org_name)

    def to_urn(self):
        return f'{uc_start}{self.codetype}:{self.org}:{self.code}'
