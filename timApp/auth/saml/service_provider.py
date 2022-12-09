from dataclasses import field, dataclass
from typing import Callable
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


@dataclass(frozen=True, slots=True)
class IdpDescription:
    display_names: dict[str, str]
    scopes: list[str] = field(default_factory=list)


@dataclass
class SamlService:
    name: str
    metadata_url: str

    get_idp_description: Callable[[str, dict], IdpDescription | None]

    metadata_timeout: int = 60 * 60 * 24

    @property
    def metadata_cache_key(self) -> str:
        return f"{self.name}_{self.metadata_url}_metadata"

    @property
    def metadata(self) -> bytes:
        metadata = cache.get(self.metadata_cache_key)
        if metadata:
            return metadata
        try:
            response = requests.get(self.metadata_url)
            response.raise_for_status()
        except requests.exceptions.RequestException as e:
            raise RouteException(
                f"Could not fetch IDP metadata from {self.metadata_url}: {e}"
            )
        cache.set(
            self.metadata_cache_key, response.content, timeout=self.metadata_timeout
        )
        return response.content

    def init_saml_client(self) -> Saml2Client:
        return get_saml_client(self.name, lambda: self.metadata)

    def init_saml_config(self) -> Saml2Config:
        return get_saml_config(self.name, lambda: self.metadata)


def _handle_sso(
    service: SamlService,
    return_to: str,
    entity_id: str,
    debug: bool,
    add_user: bool,
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

    client = service.init_saml_client()
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


def _handle_acs(service: SamlService) -> Response:
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

    client = service.init_saml_client()

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


def _get_idps(service: SamlService) -> list[dict]:
    cache_key = f"{service.metadata_cache_key}_feed_json"
    feed = cache.get(cache_key)
    if feed:
        return feed
    feed = []
    config = service.init_saml_config()
    meta: MetadataStore = config.metadata
    idps = meta.with_descriptor("idpsso")
    for entity_id, idp_info in idps.items():
        idp_desc = service.get_idp_description(entity_id, idp_info)
        if not idp_desc:
            continue

        feed.append(
            {
                "entityID": entity_id,
                "displayNames": [
                    {
                        "lang": lang,
                        "value": name,
                    }
                    for lang, name in idp_desc.display_names.items()
                ],
                "scopes": idp_desc.scopes,
            }
        )

    cache.set(cache_key, feed, timeout=service.metadata_timeout)
    return feed


def add_saml_sp_routes(saml_bp: TypedBlueprint, service: SamlService) -> TypedBlueprint:
    @saml_bp.get("")
    def get_metadata() -> Response:
        saml_config = service.init_saml_config()
        try:
            resp = make_response(create_metadata_string(None, config=saml_config), 200)
            resp.headers["Content-Type"] = "text/xml"
        except SAMLError as e:
            log_warning(f"Could not create SAML metadata: {e}")
            resp = make_response(f"Could not create SAML metadata: {e}", 400)

        return resp

    @saml_bp.get("/feed")
    def get_idp_feed() -> Response:
        return json_response(_get_idps(service))

    @csrf.exempt
    @saml_bp.post("/acs")
    def acs() -> Response:
        return _handle_acs(service)

    @saml_bp.get("/sso")
    def sso(
        return_to: str,
        entity_id: str = field(metadata={"data_key": "entityID"}),
        debug: bool = False,
        add_user: bool = field(default=False, metadata={"data_key": "addUser"}),
    ) -> Response:
        return _handle_sso(service, return_to, entity_id, debug, add_user)

    return saml_bp
