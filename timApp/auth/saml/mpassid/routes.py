from timApp.auth.saml.service_provider import (
    add_saml_sp_routes,
    SamlService,
    IdpDescription,
    BaseSamlUserAttributes,
)
from timApp.user.personaluniquecode import UserPersonalUniqueCode
from timApp.user.user import UserOrigin
from timApp.util.flask.typedblueprint import TypedBlueprint


def _get_mpassid_idp_desc(entity_id: str, idp_info: dict) -> IdpDescription | None:
    return IdpDescription({"fi": "MPASSid", "en": "MPASSid", "sv": "MPASSid"}, [])


class MpassidSamlUserAttributes(BaseSamlUserAttributes):
    @property
    def derived_username(self) -> str:
        pass

    @property
    def surname(self) -> str:
        return self.get_attribute("family_name", str)

    @property
    def given_name(self) -> str:
        return self.get_attribute("given_name", str)

    @property
    def email(self) -> str:
        return ""

    @property
    def unique_codes(self) -> list[UserPersonalUniqueCode] | None:
        return [
            UserPersonalUniqueCode(
                code=self.get_attribute("uid", str),
                codetype="userID",
                org="MPASSid",
            )
        ]

    @property
    def organisation_group(self) -> str | None:
        return None


saml_mpassid = add_saml_sp_routes(
    TypedBlueprint("saml_mpassid", __name__, url_prefix="/saml/mpass"),
    SamlService(
        name="mpassid",
        metadata_url="https://mpass-proxy-test.csc.fi/idp/shibboleth",
        user_origin=UserOrigin.Haka,
        service_group_name="MPASSid users",
        get_idp_description=_get_mpassid_idp_desc,
        saml_user_attributes_type=MpassidSamlUserAttributes,
        metadata_timeout=60 * 60 * 24,
    ),
)
