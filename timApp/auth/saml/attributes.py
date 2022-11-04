import functools
from dataclasses import dataclass
from typing import Any, Optional

from timApp.auth.saml.identity_assurance import (
    IdentityAssuranceProofing,
    RefedsIapLevel,
    REFEDS_LOCAL_ENTRPRISE_ASSURANCE,
    _T,
)
from timApp.tim_app import app
from timApp.util.flask.requesthelper import RouteException


@dataclass
class SAMLUserAttributes:
    response_attribues: dict[str, Any]

    def _get_attribute_safe(self, name: str) -> Optional[_T]:
        return self.response_attribues.get(name, [None])[0]

    def _get_attribute(self, name: str) -> _T:
        value = self._get_attribute_safe(name)
        if value is None:
            raise RouteException(f"Missing required attribute {name}")
        return value

    @property
    def common_name(self) -> str:
        return self._get_attribute("cn")

    @property
    def email(self) -> str:
        return self._get_attribute("mail")

    @property
    def surname(self) -> str:
        return self._get_attribute("sn")

    @property
    def given_name(self) -> str:
        return self._get_attribute("givenName")

    @property
    def display_name(self) -> str:
        return self._get_attribute("displayName")

    @property
    def edu_person_principal_name(self) -> str:
        return self._get_attribute("eduPersonPrincipalName")

    @property
    def edu_person_assurance(self) -> list[str]:
        res = self.response_attribues.get("eduPersonAssurance")
        if not isinstance(res, list):
            raise RouteException("eduPersonAssurance is not a list")
        return res

    @property
    def preferred_language(self) -> str:
        return self._get_attribute("preferredLanguage")

    @property
    def eppn_parts(self) -> list[str]:
        return self.edu_person_principal_name.split("@")

    @property
    def org(self) -> str:
        return self.eppn_parts[1]

    @property
    def unique_codes(self) -> list[str] | None:
        return self.response_attribues.get("schacPersonalUniqueCode")

    @property
    def derived_username(self) -> str:
        uname, org = self.eppn_parts
        if org == app.config["HOME_ORGANIZATION"]:
            return uname
        return f"{org}:{uname}"

    @functools.cached_property
    def identity_assurance_proofing(self) -> IdentityAssuranceProofing:
        """Parses and returns the best Identity Assurance Proofing (IAP) level
        from eduPersonAssurance based on REFEDS Assurance Framework ver 1.0 spec.

        :return: Identity assurence proofing level
        """
        iap_levels = [
            RefedsIapLevel.from_string(level) for level in self.edu_person_assurance
        ]
        # Don't check default because IAP must always be provided
        best_level = max(level for level in iap_levels if level is not None)
        return IdentityAssuranceProofing(
            best_level, REFEDS_LOCAL_ENTRPRISE_ASSURANCE in self.edu_person_assurance
        )

    def to_json(self) -> dict[str, Any]:
        return {
            "cn": self.common_name,
            "displayName": self.display_name,
            "eduPersonPrincipalName": self.edu_person_principal_name,
            "givenName": self.given_name,
            "mail": self.email,
            "preferredLanguage": self.preferred_language,
            "sn": self.surname,
            "schacPersonalUniqueCode": self.unique_codes,
        }
