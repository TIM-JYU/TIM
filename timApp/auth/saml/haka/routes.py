from timApp.auth.saml.service_provider import (
    add_saml_sp_routes,
    SamlService,
    IdpDescription,
)
from timApp.tim_app import app
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


saml_haka = add_saml_sp_routes(
    TypedBlueprint("saml", __name__, url_prefix="/saml"),
    SamlService(
        name="haka",
        metadata_url=app.config["HAKA_METADATA_URL"],
        get_idp_description=_get_haka_idp_desc,
        metadata_timeout=60 * 60 * 24,
    ),
)
