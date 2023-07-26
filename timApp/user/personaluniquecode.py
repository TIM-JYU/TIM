import re
from dataclasses import dataclass
from typing import Optional

from sqlalchemy import select
from sqlalchemy.orm import mapped_column

from timApp.timdb.sqa import db
from timApp.user.hakaorganization import HakaOrganization


class PersonalUniqueCode(db.Model):
    """The database model for the 'schacPersonalUniqueCode' Haka attribute."""

    

    user_id = mapped_column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False, primary_key=True
    )
    """User id."""

    org_id = mapped_column(
        db.Integer,
        db.ForeignKey("haka_organization.id"),
        nullable=False,
        primary_key=True,
    )
    """Organization id."""

    code = mapped_column(db.Text, nullable=False, index=True)
    """The actual code. This could be e.g. student id or employee id."""

    type = mapped_column(db.Text, nullable=False, primary_key=True)
    """The type of the code, e.g. student or employee."""

    user = db.relationship("User", back_populates="uniquecodes", lazy="selectin")
    organization = db.relationship(
        "HakaOrganization", back_populates="uniquecodes", lazy="selectin"
    )

    __table_args__ = (db.UniqueConstraint("org_id", "code", "type"),)

    @property
    def user_collection_key(self):
        return self.org_id, self.type

    @staticmethod
    def find_by_student_id(sid: str, org: str) -> Optional["PersonalUniqueCode"]:
        return PersonalUniqueCode.find_by_code(sid, org, "studentID")

    @staticmethod
    def find_by_code(
        code: str, org: str, codetype: str
    ) -> Optional["PersonalUniqueCode"]:
        return (
            db.session.execute(
                select(PersonalUniqueCode)
                .filter_by(code=code, type=codetype)
                .join(HakaOrganization)
                .filter_by(name=org)
                .limit(1)
            )
            .scalars()
            .first()
        )

    @staticmethod
    def find_by_urn(urn: str) -> Optional["PersonalUniqueCode"]:
        p = SchacPersonalUniqueCode.parse(urn)
        if not p:
            return None
        return PersonalUniqueCode.find_by_code(p.code, p.org, p.codetype)


uc_start = "urn:schac:personalUniqueCode:int"

# The mace:terena.org part is used by at least Aalto.
uc_re = re.compile(
    rf"urn:(mace:terena\.org:)?schac:personalUniqueCode:int:(?P<type>[^:]+):(?P<org>[^:]+):(?P<code>[^:]+)"
)


@dataclass
class SchacPersonalUniqueCode:
    """Represents the 'schacPersonalUniqueCode' Haka attribute."""

    code: str
    codetype: str
    org: str

    @staticmethod
    def parse(urn: str):
        match = uc_re.fullmatch(urn)
        if not match:
            return None
        codetype = match.group("type")
        org_name = match.group("org")
        code = match.group("code")
        return SchacPersonalUniqueCode(code=code, codetype=codetype, org=org_name)

    def to_urn(self):
        return f"{uc_start}:{self.codetype}:{self.org}:{self.code}"
