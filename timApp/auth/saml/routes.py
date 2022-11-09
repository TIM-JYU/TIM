from dataclasses import field
from xml.etree.ElementTree import ParseError

import requests
from flask import make_response, session, request, url_for, redirect
from saml2 import SAMLError, BINDING_HTTP_POST
from saml2.client import Saml2Client
from saml2.config import Config as Saml2Config
from saml2.mdstore import MetadataStore
from saml2.metadata import create_metadata_string
from saml2.response import AuthnResponse
from saml2.s_utils import SamlException
from werkzeug.sansio.response import Response

from timApp.auth.accesshelper import AccessDenied
from timApp.auth.login import create_or_update_user, set_user_to_session
from timApp.auth.saml.attributes import SAMLUserAttributes
from timApp.auth.saml.client import (
    get_saml_config,
    get_saml_client,
)
from timApp.auth.sessioninfo import logged_in
from timApp.tim_app import app, csrf
from timApp.timdb.sqa import db
from timApp.user.personaluniquecode import SchacPersonalUniqueCode
from timApp.user.user import UserInfo, UserOrigin
from timApp.user.usercontact import ContactOrigin
from timApp.user.usergroup import UserGroup
from timApp.util.error_handlers import report_error
from timApp.util.flask.cache import cache
from timApp.util.flask.requesthelper import RouteException, is_testing
from timApp.util.flask.responsehelper import json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_warning

saml = TypedBlueprint("saml", __name__, url_prefix="/saml")
"""
Blueprint for SAML routes.
"""


@cache.memoize(timeout=3600 * 24)
def _get_idp_metadata_from_url(url: str) -> bytes:
    try:
        response = requests.get(url)
        response.raise_for_status()
        return response.content
    except requests.exceptions.RequestException as e:
        raise RouteException(f"Could not fetch IDP metadata from {url}: {e}")


def _get_haka_metadata() -> bytes:
    return _get_idp_metadata_from_url(app.config["HAKA_METADATA_URL"])


def _get_saml_client() -> Saml2Client:
    return get_saml_client(_get_haka_metadata)


def _get_saml_config() -> Saml2Config:
    return get_saml_config(_get_haka_metadata)


@saml.get("/sso")
def sso(
    return_to: str,
    entity_id: str = field(metadata={"data_key": "entityID"}),
    debug: bool = False,
    add_user: bool = field(default=False, metadata={"data_key": "addUser"}),
) -> Response:
    """
    Perform single sign-on to TIM via the SAML2 protocol.
    This is the starting point of SAML login once the IdP has been selected.

    :param return_to: What URL to return to after successful login?
    :param entity_id: IdP entity ID that will provide authentication
    :param debug: If true, return debug information instead of redirecting to return_to
    :param add_user: Whether to add the logged-in user to the session instead of replacing it
    :return: SSO redirect
    """
    if not logged_in() and add_user:
        raise AccessDenied("You must be logged in before adding users to session.")

    client = _get_saml_client()
    req_id, info = client.prepare_for_authenticate(entity_id, relay_state=return_to)
    redirect_url = next(v for k, v in info["headers"] if k == "Location")

    session["entityID"] = entity_id
    session["adding_user"] = add_user
    session["requestID"] = req_id
    session["cameFrom"] = request.base_url
    if debug:
        session["debugSSO"] = True
    else:
        session.pop("debugSSO", None)
    return redirect(redirect_url)


def _get_saml_response(
    client: Saml2Client, request_id: str, came_from: str
) -> AuthnResponse:
    saml_response = request.form.get("SAMLResponse")
    if not saml_response:
        raise SamlException("SAML Response is missing")
    return client.parse_authn_request_response(
        saml_response,
        BINDING_HTTP_POST,
        outstanding={request_id: came_from},
    )


@csrf.exempt
@saml.post("/acs")
def acs() -> Response:
    """
    Handle Assertion Consumer Service (ACS) response from IdP.
    Finalise the SSO process and log the user in.
    If the user doesn't yet exist in the database, create it.

    :return: Redirect to return_to URL or debug information depending on the information provided during /sso
    """
    entity_id = session.get("entityID")
    came_from = session.get("cameFrom")
    request_id = session.get("requestID")
    if not entity_id:
        raise RouteException("No entityID in session.")
    if not came_from:
        raise RouteException("No cameFrom in session.")
    if not request_id:
        raise RouteException("No requestID in session.")

    client = _get_saml_client()

    try:
        resp = _get_saml_response(client, request_id, came_from)
    except (SamlException, ParseError, SAMLError) as e:
        report_error(f"Error parsing SAML response: {e}", with_http_body=True)
        if is_testing():
            raise RouteException(str(e))
        raise RouteException(
            f"Error parsing SAML response. You can log in using your TIM username and password instead. "
            f"Please contact {app.config['HELP_EMAIL']} if the problem persists."
        )

    ava = resp.get_identity()

    session.pop("requestID", None)
    session.pop("cameFrom", None)

    saml_attributes = SAMLUserAttributes(ava)
    org_group = UserGroup.get_organization_group(saml_attributes.org)
    parsed_codes = []
    ucs = saml_attributes.unique_codes
    if ucs:
        for c in ucs:
            parsed = SchacPersonalUniqueCode.parse(c)
            if not parsed:
                log_warning(f"Failed to parse unique code: {c}")
            else:
                parsed_codes.append(parsed)
    elif ucs is None:
        log_warning(f"{saml_attributes.derived_username} did not receive unique codes")
    else:
        log_warning(
            f"{saml_attributes.derived_username} received empty unique code list"
        )
        # Don't update email here to prevent setting is as primary automatically
    user = create_or_update_user(
        UserInfo(
            username=saml_attributes.derived_username,
            full_name=f"{saml_attributes.surname} {saml_attributes.given_name}",
            email=saml_attributes.email,
            given_name=saml_attributes.given_name,
            last_name=saml_attributes.surname,
            origin=UserOrigin.Haka,
            unique_codes=parsed_codes,
        ),
        group_to_add=org_group,
        update_email=False,  # Don't update the email here since we don't want to force the Haka mail as primary
    )
    user.set_emails(
        [saml_attributes.email], ContactOrigin.Haka, can_update_primary=True
    )
    haka = UserGroup.get_haka_group()
    if haka not in user.groups:
        user.groups.append(haka)
    db.session.commit()
    set_user_to_session(user)
    if session.get("debugSSO"):
        return json_response(ava)
    rs = request.form.get("RelayState")
    if rs:
        return redirect(rs)
    return redirect(url_for("start_page"))


@saml.get("")
def get_metadata() -> Response:
    """
    Get the SAML2 metadata for this service in XML format.

    :return: SAML2 metadata of this service provider
    """
    saml_config = _get_saml_config()
    try:
        resp = make_response(create_metadata_string(None, config=saml_config), 200)
        resp.headers["Content-Type"] = "text/xml"
    except SAMLError as e:
        log_warning(f"Could not create SAML metadata: {e}")
        resp = make_response(f"Could not create SAML metadata: {e}", 400)

    return resp


@saml.get("/feed")
def get_idps() -> Response:
    """
    Get a list of IdPs available for login based on active metadata.

    :return: JSON list of IdPs usable for login.
    """
    config = _get_saml_config()
    meta: MetadataStore = config.metadata
    idps = meta.with_descriptor("idpsso")
    feed = []
    for entity_id, idp_info in idps.items():
        sso_desc = idp_info.get("idpsso_descriptor")
        if not sso_desc:
            log_warning(f"SAML: Could not find SSO info for {entity_id}")
            continue
        ext_elems = sso_desc[0].get("extensions", {}).get("extension_elements", None)
        if ext_elems is None:
            log_warning(f"SAML: Could not find extension elements for {entity_id}")
            continue
        ui_info = next(
            (d for d in ext_elems if d["__class__"].endswith("ui&UIInfo")), None
        )
        if ui_info is None:
            log_warning(f"SAML: Could not find UIInfo for {entity_id}")
            continue
        if not isinstance(ui_info, dict):
            log_warning(f"SAML: UIInfo is not a dict for {entity_id}")
            continue
        # get display names
        display_names = [
            {
                "value": name_entry["text"],
                "lang": name_entry["lang"],
            }
            for name_entry in ui_info["display_name"]
        ]
        # Check for scope extension
        scopes = []
        scope_elems = [e for e in ext_elems if e["__class__"].endswith("&Scope")]
        for ext_elem in scope_elems:
            scopes.append(ext_elem["text"])

        feed.append(
            {
                "entityID": entity_id,
                "displayNames": display_names,
                "scopes": scopes,
            }
        )

    return json_response(feed)
