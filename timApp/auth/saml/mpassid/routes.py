from timApp.auth.saml.service_provider import (
    add_saml_sp_routes,
    SamlService,
    IdpDescription,
    BaseSamlUserAttributes,
)
from timApp.user.personaluniquecode import SchacPersonalUniqueCode
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
        pass

    @property
    def given_name(self) -> str:
        pass

    @property
    def email(self) -> str:
        pass

    @property
    def unique_codes(self) -> list[SchacPersonalUniqueCode] | None:
        pass

    @property
    def organisation_group(self) -> str | None:
        pass


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
