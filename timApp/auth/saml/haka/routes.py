import functools
from typing import Any

from timApp.auth.saml.haka.iap_models import (
    IdentityAssuranceProofing,
    RefedsIapLevel,
    REFEDS_LOCAL_ENTRPRISE_ASSURANCE,
)
from timApp.auth.saml.service_provider import (
    add_saml_sp_routes,
    SamlService,
    IdpDescription,
    BaseSamlUserAttributes,
)
from timApp.tim_app import app
from timApp.user.personaluniquecode import SchacPersonalUniqueCode
from timApp.user.userorigin import UserOrigin
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_warning


def _get_haka_idp_desc(entity_id: str, idp_info: dict) -> IdpDescription | None:
    sso_desc = idp_info.get("idpsso_descriptor")
    if not sso_desc:
        log_warning(f"SAML: Could not find SSO info for {entity_id}")
        return None

    ext_elems = sso_desc[0].get("extensions", {}).get("extension_elements", None)
    if ext_elems is None:
        log_warning(f"SAML: Could not find extensions for {entity_id}")
        return None
    ui_info = next((d for d in ext_elems if d["__class__"].endswith("ui&UIInfo")), None)
    if ui_info is None:
        log_warning(f"SAML: Could not find UIInfo for {entity_id}")
        return None
    if not isinstance(ui_info, dict):
        log_warning(f"SAML: UIInfo is not a dict for {entity_id}")
        return None
    display_names = {
        name_entry["lang"]: name_entry["text"] for name_entry in ui_info["display_name"]
    }
    scope_elems = [e for e in ext_elems if e["__class__"].endswith("&Scope")]
    scopes = [e["text"] for e in scope_elems]

    return IdpDescription(display_names, scopes)


HAKA_USERS_GROUPNAME = "Haka users"


class HakaSamlUserAttributes(BaseSamlUserAttributes):
    """
    User-specific attributes returned by Haka and used by TIM.

    .. note:: The attribute names are based on the funetEduPersonSchema described in
              https://wiki.eduuni.fi/display/CSCHAKA/funetEduPersonSchema2dot4
    """

    @property
    def edu_person_principal_name(self) -> str:
        """
        A scoped identifier for a person.
        Represented in form user@scope where 'user' is a name-based identifier for the person and where the "scope"
        is the administrative domain of the identity system where the identifier was created and assigned.

        :return: A scoped identifier for a person
        """
        return self.get_attribute("eduPersonPrincipalName", str)

    @property
    def eppn_parts(self) -> list[str]:
        """
        Parts of the eduPersonPrincipalName attribute split into a list of [username, scope].

        :return: eduPersonPrincipalName attribute split into a list of [username, scope]
        """
        return self.edu_person_principal_name.split("@")

    @property
    def derived_username(self) -> str:
        uname, org = self.eppn_parts
        if org == app.config["HOME_ORGANIZATION"]:
            return uname
        return f"{org}:{uname}"

    @property
    def surname(self) -> str:
        return self.get_attribute("sn", str)

    @property
    def given_name(self) -> str:
        return self.get_attribute("givenName", str)

    @property
    def email(self) -> str:
        return self.get_attribute("mail", str)

    @property
    def unique_codes(self) -> list[SchacPersonalUniqueCode] | None:
        ucs = self.attributes.get("schacPersonalUniqueCode")
        if not ucs:
            return None
        parsed_codes = []
        for uc in ucs:
            parsed = SchacPersonalUniqueCode.parse(uc)
            if not parsed:
                log_warning(f"Failed to parse unique code: {uc}")
            else:
                parsed_codes.append(parsed)

        return parsed_codes

    @property
    def organisation_group(self) -> str | None:
        return self.eppn_parts[1]

    # FIXME: These are not used, do we even need them?
    @property
    def common_name(self) -> str:
        """
        Common name of the user.
        Generally, the name the individual has indicated as the one (s)he uses + sn

        :return: Common name of the user
        """
        return self.get_attribute("cn", str)

    # FIXME: Not used, is this needed?
    @property
    def display_name(self) -> str:
        """
        Preferred name of a person to be used when displaying entries.
        Generally, the name the individual has indicated as the one (s)he uses + sn.

        :return: Preferred name of a person to be used when displaying entries
        """
        return self.get_attribute("displayName", str)

    # FIXME: This could be a general method, but they are not yet used anywhere
    @property
    def edu_person_assurance(self) -> list[str]:
        """
        List of identity assurance profiles (IAPs) which are the set of standards that are met by an identity assertion.
        In other words, specifies the level of assurance of the user's identity that the IdP is asserting.

        .. note:: Instead, consider using :py:attr:`identity_assurance_proofing` which is a higher-level
                  abstraction over the value.

        :return: List of identity assurance profiles (IAPs)
        """
        res = self.attributes.get("eduPersonAssurance")
        if not isinstance(res, list):
            raise RouteException("eduPersonAssurance is not a list")
        return res

    # FIXME: Not used anywhere yet
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

    # FIXME: Not used anywhere
    @property
    def preferred_language(self) -> str:
        """
        Preferred written or spoken language for a person.

        :return: Preferred written or spoken language for a person
        """
        return self.get_attribute("preferredLanguage", str)

    def to_json(self) -> dict[str, Any]:
        res = super().to_json()
        return {
            **res,
            "cn": self.common_name,
            "displayName": self.display_name,
            "preferredLanguage": self.preferred_language,
            "eduPersonPrincipalName": self.edu_person_principal_name,
            "eduPersonAssurance": self.edu_person_assurance,
        }


saml_haka = add_saml_sp_routes(
    TypedBlueprint("saml", __name__, url_prefix="/saml"),
    SamlService(
        name="haka",
        metadata_url=app.config["HAKA_METADATA_URL"],
        user_origin=UserOrigin.Haka,
        service_group_name=HAKA_USERS_GROUPNAME,
        get_idp_description=_get_haka_idp_desc,
        saml_user_attributes_type=HakaSamlUserAttributes,
        metadata_timeout=60 * 60 * 24,
    ),
)
