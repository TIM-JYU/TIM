import re
from dataclasses import dataclass
from typing import Optional

from timApp.timdb.sqa import db
from timApp.user.externalorganization import ExternalOrganization


class PersonalUniqueCode(db.Model):
    """The database model for the 'schacPersonalUniqueCode' Haka attribute."""

    user_id = db.Column(
        db.Integer, db.ForeignKey("useraccount.id"), nullable=False, primary_key=True
    )
    """User id."""

    org_id = db.Column(
        db.Integer,
        db.ForeignKey("external_organization.id"),
        nullable=False,
        primary_key=True,
    )
    """Organization id."""

    code = db.Column(db.Text, nullable=False, index=True)
    """The actual code. This could be e.g. student id or employee id."""

    type = db.Column(db.Text, nullable=False, primary_key=True)
    """The type of the code, e.g. student or employee."""

    user = db.relationship("User", back_populates="uniquecodes", lazy="joined")
    organization = db.relationship(
        "ExternalOrganization", back_populates="uniquecodes", lazy="joined"
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
            PersonalUniqueCode.query.filter_by(code=code, type=codetype)
            .join(ExternalOrganization)
            .filter_by(name=org)
            .first()
        )

    @staticmethod
    def find_by_schac_urn(urn: str) -> Optional["PersonalUniqueCode"]:
        p = UserPersonalUniqueCode.parse_schac_urn(urn)
        if not p:
            return None
        return PersonalUniqueCode.find_by_code(p.code, p.org, p.codetype)


uc_start = "urn:schac:personalUniqueCode:int"

# The mace:terena.org part is used by at least Aalto.
uc_re = re.compile(
    rf"urn:(mace:terena\.org:)?schac:personalUniqueCode:int:(?P<type>[^:]+):(?P<org>[^:]+):(?P<code>[^:]+)"
)


@dataclass
class UserPersonalUniqueCode:
    """Represents the user's unique identifier within an identity provider.

    Modelled against 'schacPersonalUniqueCode' attribute.
    """

    code: str
    """
    Unique identifying code.
    """
    codetype: str
    """
    Type of the code (e.g. student or employee).
    """
    org: str
    """
    The organization that issued the code.
    """

    @staticmethod
    def parse_schac_urn(urn: str):
        """
        Parses the 'schacPersonalUniqueCode' attribute value.
        """
        match = uc_re.fullmatch(urn)
        if not match:
            return None
        codetype = match.group("type")
        org_name = match.group("org")
        code = match.group("code")
        return UserPersonalUniqueCode(code=code, codetype=codetype, org=org_name)

    def to_schac_urn(self):
        """
        Returns the 'schacPersonalUniqueCode' attribute value in URN format.
        """
        return f"{uc_start}:{self.codetype}:{self.org}:{self.code}"
