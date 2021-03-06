import functools
import json
import os
from copy import copy
from dataclasses import dataclass, field
from enum import Enum
from functools import cached_property, total_ordering
from typing import Optional, Any
from urllib.parse import urlparse

import xmlsec
from flask import request, redirect, session, make_response, Blueprint, Request, url_for
from lxml import etree
from lxml.cssselect import CSSSelector
from onelogin.saml2.auth import OneLogin_Saml2_Auth
from onelogin.saml2.errors import OneLogin_Saml2_Error, OneLogin_Saml2_ValidationError
from onelogin.saml2.idp_metadata_parser import OneLogin_Saml2_IdPMetadataParser
from onelogin.saml2.settings import OneLogin_Saml2_Settings
from onelogin.saml2.utils import OneLogin_Saml2_Utils, return_false_on_exception
from onelogin.saml2.xml_utils import OneLogin_Saml2_XML

from timApp.auth.accesshelper import AccessDenied
from timApp.auth.login import create_or_update_user, set_user_to_session
from timApp.auth.sessioninfo import logged_in
from timApp.tim_app import app, csrf
from timApp.timdb.sqa import db
from timApp.user.personaluniquecode import SchacPersonalUniqueCode
from timApp.user.user import UserInfo, UserOrigin
from timApp.user.usercontact import ContactOrigin
from timApp.user.usergroup import UserGroup
from timApp.util.flask.cache import cache
from timApp.util.flask.requesthelper import use_model, RouteException
from timApp.util.flask.responsehelper import json_response
from timApp.util.logger import log_warning

saml = Blueprint("saml", __name__, url_prefix="/saml")


class FingerPrintException(Exception):
    pass


def load_sp_settings(
    hostname=None, try_new_cert=False, sp_validation_only=False
) -> tuple[str, OneLogin_Saml2_Settings]:
    """
    Loads OneLogin Saml2 settings for the given hostname.

    Behaves like OneLogin_Saml2_Settings constructor with custom_base_path set, but allows to dynamically change
    Assertion Consumer Service URL. If the ACS URL has $hostname variable, it's replaced with the given hostname
    argument.

    .. note: Templating is used because OneLogin does not support multiple ACSs at the moment, see
                https://github.com/onelogin/python-saml/issues/207

    :param hostname: Hostname for which to generate the ACS callback URL. If None, TIM_HOST is used.
    :param try_new_cert: If True, settings and certificate from /new folder is used.
    :param sp_validation_only: If True, the SP settings are only validated.
    :return: Tuple (str, OneLogin_Saml2_Settings) contains path to current Saml2 settings and generated SP settings
                object
    """
    saml_path = app.config["SAML_PATH"]
    if try_new_cert:
        saml_path += "/new"

    filename = os.path.join(saml_path, "settings.json")
    try:
        with open(filename) as json_data:
            settings = json.loads(json_data.read())
    except FileNotFoundError:
        raise OneLogin_Saml2_Error(
            "Settings file not found: %s",
            OneLogin_Saml2_Error.SETTINGS_FILE_NOT_FOUND,
            filename,
        )

    try:
        advanced_filename = os.path.join(saml_path, "advanced_settings.json")
        with open(advanced_filename) as json_data:
            settings.update(json.loads(json_data.read()))
    except FileNotFoundError:
        pass

    acs_info = settings["sp"]["assertionConsumerService"]
    acs_info["url"] = acs_info["url"].replace(
        "$hostname", hostname or app.config["TIM_HOST"]
    )

    ol_settings = OneLogin_Saml2_Settings(
        settings=settings,
        custom_base_path=saml_path,
        sp_validation_only=sp_validation_only,
    )
    return saml_path, ol_settings


@return_false_on_exception
def validate_node_sign(
    signature_node,
    elem,
    cert=None,
    fingerprint=None,
    fingerprintalg="sha1",
    validatecert=False,
    debug=False,
):
    """
    Same as OneLogin_Saml2_Utils.validate_node_sign but with the following changes:

    * If the certificate fingerprint does not match, an exception is raised (to make debugging easier).
    """
    if (cert is None or cert == "") and fingerprint:
        x509_certificate_nodes = OneLogin_Saml2_XML.query(
            signature_node, "//ds:Signature/ds:KeyInfo/ds:X509Data/ds:X509Certificate"
        )
        if len(x509_certificate_nodes) > 0:
            x509_certificate_node = x509_certificate_nodes[0]
            x509_cert_value = OneLogin_Saml2_XML.element_text(x509_certificate_node)
            x509_cert_value_formatted = OneLogin_Saml2_Utils.format_cert(
                x509_cert_value
            )
            x509_fingerprint_value = OneLogin_Saml2_Utils.calculate_x509_fingerprint(
                x509_cert_value_formatted, fingerprintalg
            )
            if fingerprint == x509_fingerprint_value:
                cert = x509_cert_value_formatted
            else:
                raise FingerPrintException(
                    f"Expected certificate fingerprint {fingerprint} but got {x509_fingerprint_value}"
                )

    if cert is None or cert == "":
        raise OneLogin_Saml2_Error(
            "Could not validate node signature: No certificate provided.",
            OneLogin_Saml2_Error.CERT_NOT_FOUND,
        )

    if validatecert:
        manager = xmlsec.KeysManager()
        manager.load_cert_from_memory(
            cert, xmlsec.KeyFormat.CERT_PEM, xmlsec.KeyDataType.TRUSTED
        )
        dsig_ctx = xmlsec.SignatureContext(manager)
    else:
        dsig_ctx = xmlsec.SignatureContext()
        dsig_ctx.key = xmlsec.Key.from_memory(cert, xmlsec.KeyFormat.CERT_PEM, None)

    dsig_ctx.set_enabled_key_data([xmlsec.KeyData.X509])

    try:
        dsig_ctx.verify(signature_node)
    except Exception as err:
        raise OneLogin_Saml2_ValidationError(
            "Signature validation failed. %s",
            OneLogin_Saml2_ValidationError.INVALID_SIGNATURE,
            str(err),
        )

    return True


OneLogin_Saml2_Utils.validate_node_sign = validate_node_sign


def do_validate_metadata(idp_metadata_xml: str, fingerprint: str) -> None:
    try:
        if not OneLogin_Saml2_Utils.validate_metadata_sign(
            idp_metadata_xml,
            validatecert=False,
            fingerprint=fingerprint,
            raise_exceptions=True,
            fingerprintalg="sha256",
        ):
            raise RouteException("Failed to validate Haka metadata")
    except OneLogin_Saml2_ValidationError as e:
        raise RouteException(f"Failed to validate Haka metadata: {e}")


def init_saml_auth(req, entity_id: str, try_new_cert: bool) -> OneLogin_Saml2_Auth:
    idp_metadata_xml = get_haka_metadata()
    idp_data = OneLogin_Saml2_IdPMetadataParser.parse(
        idp_metadata_xml, entity_id=entity_id
    )
    if "idp" not in idp_data:
        raise RouteException(f"IdP not found from Haka metadata: {entity_id}")
    try:
        do_validate_metadata(idp_metadata_xml, app.config["HAKA_METADATA_FINGERPRINT"])
    except FingerPrintException as e:
        log_warning(f"{e} - trying with new fingerprint")
        try:
            do_validate_metadata(
                idp_metadata_xml, app.config["HAKA_METADATA_FINGERPRINT_NEW"]
            )
        except FingerPrintException as e:
            raise RouteException(f"Failed to validate Haka metadata: {e}")

    saml_path, osett = load_sp_settings(
        req["http_host"], try_new_cert, sp_validation_only=True
    )
    sp = osett.get_sp_data()

    settings = OneLogin_Saml2_IdPMetadataParser.merge_settings({"sp": sp}, idp_data)
    auth = OneLogin_Saml2_Auth(req, settings, custom_base_path=saml_path)
    return auth


def get_haka_metadata() -> str:
    return get_haka_metadata_from_url(app.config["HAKA_METADATA_URL"])


@cache.memoize(timeout=3600 * 24)
def get_haka_metadata_from_url(url: str) -> str:
    idp_metadata_xml = OneLogin_Saml2_IdPMetadataParser.get_metadata(url)
    return idp_metadata_xml


def prepare_flask_request(r: Request):
    url_data = urlparse(r.url)
    return {
        "https": "on",
        "http_host": r.host,
        "server_port": url_data.port,
        "script_name": r.path,
        "get_data": r.args.copy(),
        "post_data": r.form.copy(),
    }


@dataclass
class SSOData:
    return_to: str
    entityID: str
    debug: bool = False
    addUser: bool = False


def prepare_and_init(entity_id: str, try_new_cert: bool) -> OneLogin_Saml2_Auth:
    req = prepare_flask_request(request)
    auth = init_saml_auth(req, entity_id, try_new_cert)
    return auth


REFEDS_PREFIX = "https://refeds.org/assurance"
REFEDS_LOCAL_ENTRPRISE_ASSURANCE = f"{REFEDS_PREFIX}/IAP/local-enterprise"


@total_ordering
class RefedsIapLevel(Enum):
    """Valid Identity Assurance Proofing levels for REFEDS Assurance Framework ver 1.0 based on
    https://wiki.refeds.org/display/ASS/REFEDS+Assurance+Framework+ver+1.0
    """

    low = f"{REFEDS_PREFIX}/IAP/low"
    medium = f"{REFEDS_PREFIX}/IAP/medium"
    high = f"{REFEDS_PREFIX}/IAP/high"

    @staticmethod
    @functools.cache
    def as_list():
        return list(RefedsIapLevel)

    def __lt__(self, other: Any) -> bool:
        if not isinstance(other, RefedsIapLevel):
            return NotImplemented
        return RefedsIapLevel.as_list().index(self) < RefedsIapLevel.as_list().index(
            other
        )

    @staticmethod
    def from_string(s: str) -> Optional["RefedsIapLevel"]:
        try:
            return RefedsIapLevel(s)
        except ValueError:
            return None


@dataclass(frozen=True)
class IdentityAssuranceProofing:
    """Represents user's Identity Assurance Proofing (IAP) level.
    IAP describes how the user's identity is assured (e.g. email, government ID, etc.)

    Attributes:
        highest_refeds_level: Highest IAP level according to REFEDS Assurance Framework
        local_enterprise: If True, user's identity proofing is good enough to access Home Organisations'
                          administrative systems.
    """

    highest_refeds_level: RefedsIapLevel
    local_enterprise: bool


@dataclass
class TimRequestedAttributes:
    saml_auth: OneLogin_Saml2_Auth
    friendly_name_map: dict = field(init=False)

    def __post_init__(self):
        self.friendly_name_map = {}
        settings: OneLogin_Saml2_Settings = self.saml_auth.get_settings()
        for ra in settings.get_sp_data()["attributeConsumingService"][
            "requestedAttributes"
        ]:
            self.friendly_name_map[ra["friendlyName"]] = ra["name"]

    def get_attribute_by_friendly_name(self, name: str) -> str | None:
        values = self.get_attributes_by_friendly_name(name)
        return values[0] if values else None

    def get_attributes_by_friendly_name(self, name: str) -> list[str] | None:
        return self.saml_auth.get_attribute(self.friendly_name_map[name])

    @property
    def cn(self):
        return self.get_attribute_by_friendly_name("cn")

    @property
    def mail(self):
        return self.get_attribute_by_friendly_name("mail")

    @property
    def sn(self):
        return self.get_attribute_by_friendly_name("sn")

    @property
    def display_name(self):
        return self.get_attribute_by_friendly_name("displayName")

    @property
    def edu_person_principal_name(self):
        return self.get_attribute_by_friendly_name("eduPersonPrincipalName")

    @property
    def edu_person_assurance(self) -> list[str]:
        return self.get_attributes_by_friendly_name("eduPersonAssurance")

    @property
    def given_name(self):
        return self.get_attribute_by_friendly_name("givenName")

    @property
    def preferred_language(self):
        return self.get_attribute_by_friendly_name("preferredLanguage")

    @property
    def eppn_parts(self):
        return self.edu_person_principal_name.split("@")

    @property
    def org(self):
        return self.eppn_parts[1]

    @property
    def unique_codes(self) -> list[str] | None:
        return self.get_attributes_by_friendly_name("schacPersonalUniqueCode")

    @property
    def derived_username(self):
        uname, org = self.eppn_parts
        if org == app.config["HOME_ORGANIZATION"]:
            return uname
        return f"{org}:{uname}"

    @cached_property
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

    def to_json(self):
        return {
            "cn": self.cn,
            "displayName": self.display_name,
            "eduPersonPrincipalName": self.edu_person_principal_name,
            "givenName": self.given_name,
            "mail": self.mail,
            "preferredLanguage": self.preferred_language,
            "sn": self.sn,
            "schacPersonalUniqueCode": self.unique_codes,
        }


@saml.get("/sso")
@use_model(SSOData)
def sso(m: SSOData):
    try:
        auth = prepare_and_init(m.entityID, try_new_cert=True)
    except OneLogin_Saml2_Error:
        auth = prepare_and_init(m.entityID, try_new_cert=False)
    session["entityID"] = m.entityID
    login_url = auth.login(return_to=m.return_to)
    session["requestID"] = auth.get_last_request_id()
    if not logged_in() and m.addUser:
        raise AccessDenied("You must be logged in before adding users to session.")
    session["adding_user"] = m.addUser
    if m.debug:
        session["debugSSO"] = True
    else:
        session.pop("debugSSO", None)
    return redirect(login_url)


@csrf.exempt
@saml.post("/acs")
def acs():
    entity_id = session.get("entityID")
    if not entity_id:
        raise RouteException("entityID not in session")
    try:
        auth = try_process_saml_response(entity_id, try_new_cert=True)
    except (SamlProcessingError, OneLogin_Saml2_Error) as e:
        # OneLogin_Saml2_Error happens if there is no new certificate in the file system. That means there is no
        # rollover going on, so nothing interesting is happening.
        #
        # If instead we get a SamlProcessingError, that means there is a new certificate, but the IdP encrypted
        # the SAML response with the old certificate, so we should account for that.
        if isinstance(e, SamlProcessingError):
            log_warning(
                f"Failed to process SAML response with the new certificate; trying with old."
            )
        try:
            auth = try_process_saml_response(entity_id, try_new_cert=False)
        except SamlProcessingError as e:
            raise RouteException(str(e))
    errors = auth.get_errors()
    if not auth.is_authenticated():
        err = f"Authentication failed: {auth.get_last_error_reason()}"
        log_warning(err)
        raise RouteException(
            f'{err} (Please contact {app.config["HELP_EMAIL"]} if the problem persists.)'
        )
    if errors:
        err = str(errors)
        log_warning(err)
        raise RouteException(err)
    session.pop("requestID", None)
    timattrs = TimRequestedAttributes(auth)
    org_group = UserGroup.get_organization_group(timattrs.org)
    parsed_codes = []
    ucs = timattrs.unique_codes
    if ucs:
        for c in ucs:
            parsed = SchacPersonalUniqueCode.parse(c)
            if not parsed:
                log_warning(f"Failed to parse unique code: {c}")
            else:
                parsed_codes.append(parsed)
    elif ucs is None:
        log_warning(f"{timattrs.derived_username} did not receive unique codes")
    else:
        log_warning(f"{timattrs.derived_username} received empty unique code list")
    # Don't update email here to prevent setting is as primary automatically
    user = create_or_update_user(
        UserInfo(
            username=timattrs.derived_username,
            full_name=f"{timattrs.sn} {timattrs.given_name}",
            email=timattrs.mail,
            given_name=timattrs.given_name,
            last_name=timattrs.sn,
            origin=UserOrigin.Haka,
            unique_codes=parsed_codes,
        ),
        group_to_add=org_group,
        update_email=False,  # Don't update the email here since we don't want to force the Haka mail as primary
    )
    user.set_emails([timattrs.mail], ContactOrigin.Haka, can_update_primary=True)
    haka = UserGroup.get_haka_group()
    if haka not in user.groups:
        user.groups.append(haka)
    db.session.commit()
    set_user_to_session(user)
    if session.get("debugSSO"):
        return json_response(auth.get_attributes())
    rs = request.form.get("RelayState")
    if rs:
        return redirect(auth.redirect_to(rs))
    return redirect(url_for("start_page"))


class SamlProcessingError(Exception):
    pass


def try_process_saml_response(entity_id: str, try_new_cert: bool):
    auth = prepare_and_init(entity_id, try_new_cert)
    request_id = session.get("requestID")
    if not request_id:
        err = "requestID missing from session"
        log_warning(err)
        raise RouteException(err)
    try:
        auth.process_response(request_id=request_id)
    except Exception as e:
        err = f"Error processing SAML response: {str(e)}"
        log_warning(err)
        raise SamlProcessingError(err)
    return auth


@saml.get("")
def get_metadata():
    _, settings = load_sp_settings(request.host, sp_validation_only=True)
    metadata = settings.get_sp_metadata()
    errors = settings.validate_metadata(metadata)

    if len(errors) == 0:
        resp = make_response(metadata, 200)
        resp.headers["Content-Type"] = "text/xml"
    else:
        resp = make_response(", ".join(errors), 400)
    return resp


@saml.get("/feed")
def get_idps():
    idp_metadata_xml = get_haka_metadata()
    root = etree.fromstring(idp_metadata_xml)
    nsmap = copy(root.nsmap)
    rootns = nsmap.pop(None)
    nsmap["xhtml"] = rootns
    select_idps = CSSSelector(
        "xhtml|IDPSSODescriptor",
        namespaces=nsmap,
    )
    select_displaynames = CSSSelector(
        "mdui|DisplayName",
        namespaces=nsmap,
    )
    feed = []
    for idp in select_idps(root):
        names = []
        for n in select_displaynames(idp):
            names.append(
                {
                    "value": n.text,
                    "lang": n.attrib["{http://www.w3.org/XML/1998/namespace}lang"],
                }
            )
        scopes = []
        nsmap = idp.nsmap
        nsmap.pop(None)
        for n in CSSSelector(
            "shibmd|Scope",
            namespaces=nsmap,
        )(idp):
            scopes.append(n.text)
        feed.append(
            {
                "entityID": idp.getparent().attrib["entityID"],
                "displayNames": names,
                "scopes": scopes,
            }
        )
    return json_response(feed)
