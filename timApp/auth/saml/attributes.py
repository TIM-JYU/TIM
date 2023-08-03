import functools
from dataclasses import dataclass
from typing import Any, Optional

from timApp.auth.saml.identity_assurance import (
    IdentityAssuranceProofing,
    RefedsIapLevel,
    REFEDS_LOCAL_ENTRPRISE_ASSURANCE,
)
from timApp.tim_app import app
from timApp.util.flask.requesthelper import RouteException


@dataclass
class SAMLUserAttributes:
    """
    User-specific attributes returned by the IdP and used by TIM.

    .. note:: The attribute names are based on the funetEduPersonSchema described in
              https://wiki.eduuni.fi/display/CSCHAKA/funetEduPersonSchema2dot4
    """

    response_attributes: dict[str, Any]
    """
    Raw attribute data
    """

    def _get_attribute_safe(self, name: str) -> Optional[Any]:
        return self.response_attributes.get(name, [None])[0]

    def _get_attribute(self, name: str) -> Any:
        value = self._get_attribute_safe(name)
        if value is None:
            raise RouteException(f"Missing required attribute {name}")
        return value

    @property
    def common_name(self) -> str:
        """
        Common name of the user.
        Generally, the name the individual has indicated as the one (s)he uses + sn

        :return: Common name of the user
        """
        return self._get_attribute("cn")

    @property
    def email(self) -> str:
        """
        Email address of the user.
        Generally, the preferred address for the "to:" field of email to be sent to this person.

        :return: Email address of the user
        """
        return self._get_attribute("mail")

    @property
    def surname(self) -> str:
        """
        Surname of the user.

        :return: Surname of the user
        """
        return self._get_attribute("sn")

    @property
    def given_name(self) -> str:
        """
        Given name of the user.
        Generally, the preferred given name the person has indicated to be used.

        :return: Given name of the user
        """
        return self._get_attribute("givenName")

    @property
    def display_name(self) -> str:
        """
        Preferred name of a person to be used when displaying entries.
        Generally, the name the individual has indicated as the one (s)he uses + sn.

        :return: Preferred name of a person to be used when displaying entries
        """
        return self._get_attribute("displayName")

    @property
    def edu_person_principal_name(self) -> str:
        """
        A scoped identifier for a person.
        Represented in form user@scope where 'user' is a name-based identifier for the person and where the "scope"
        is the administrative domain of the identity system where the identifier was created and assigned.

        :return: A scoped identifier for a person
        """
        return self._get_attribute("eduPersonPrincipalName")

    @property
    def edu_person_assurance(self) -> list[str]:
        """
        List of identity assurance profiles (IAPs) which are the set of standards that are met by an identity assertion.
        In other words, specifies the level of assurance of the user's identity that the IdP is asserting.

        .. note:: Instead, consider using :py:attr:`identity_assurance_proofing` which is a higher-level
                  abstraction over the value.

        :return: List of identity assurance profiles (IAPs)
        """
        res = self.response_attributes.get("eduPersonAssurance")
        if not isinstance(res, list):
            raise RouteException("eduPersonAssurance is not a list")
        return res

    @property
    def preferred_language(self) -> str:
        """
        Preferred written or spoken language for a person.

        :return: Preferred written or spoken language for a person
        """
        return self._get_attribute("preferredLanguage")

    @property
    def eppn_parts(self) -> list[str]:
        """
        Parts of the eduPersonPrincipalName attribute split into a list of [username, scope].

        :return: eduPersonPrincipalName attribute split into a list of [username, scope]
        """
        return self.edu_person_principal_name.split("@")

    @property
    def org(self) -> str:
        """
        User's organization. This is essentially the "scope" value of eduPersonPrincipalName.

        :return: User's organization in host format
        """
        return self.eppn_parts[1]

    @property
    def unique_codes(self) -> list[str] | None:
        """
        A list of "unique codes" of the user. The values are meant to be unique and allow to identify the user in
        the given organization.

        :return: A list of unique codes of the user
        """
        return self.response_attributes.get("schacPersonalUniqueCode")

    @property
    def derived_username(self) -> str:
        """
        TIM username derived from the attributes.

        :return: The TIM username of the user
        """
        uname, org = self.eppn_parts
        if org == app.config["HOME_ORGANIZATION"]:
            return uname
        return f"{org}:{uname}"

    @functools.cached_property
    def identity_assurance_proofing(self) -> IdentityAssuranceProofing:
        """
        The best Identity Assurance Proofing (IAP) level
        that the user has based on the REFEDS Assurance Framework ver 1.0 spec.

        :return: Identity assurance proofing level
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
        """
        Convert the attributes to a JSON-serializable dictionary.
        :return: JSON-serializable dictionary of attributes
        """
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
