from copy import copy
from typing import Dict
from urllib.parse import urlparse

import xmlsec
from dataclasses import dataclass, field
from flask import request, redirect, session, make_response, Blueprint, Request
from lxml import etree
from lxml.cssselect import CSSSelector
from onelogin.saml2.auth import OneLogin_Saml2_Auth
from onelogin.saml2.errors import OneLogin_Saml2_Error, OneLogin_Saml2_ValidationError
from onelogin.saml2.idp_metadata_parser import OneLogin_Saml2_IdPMetadataParser
from onelogin.saml2.settings import OneLogin_Saml2_Settings
from onelogin.saml2.utils import OneLogin_Saml2_Utils, return_false_on_exception
from onelogin.saml2.xml_utils import OneLogin_Saml2_XML

from timApp.tim_app import app, csrf
from timApp.util.flask.requesthelper import use_model, RouteException
from timApp.util.flask.responsehelper import json_response

saml = Blueprint('saml',
                 __name__,
                 url_prefix='/saml')


class FingerPrintException(Exception):
    pass


@return_false_on_exception
def validate_node_sign(signature_node, elem, cert=None, fingerprint=None, fingerprintalg='sha1', validatecert=False,
                       debug=False):
    """
    Same as OneLogin_Saml2_Utils.validate_node_sign but with the following changes:

    * The empty Reference URI handling has been removed (Haka metadata validation does not work with it).
    * If the certificate fingerprint does not match, an exception is raised.
    """
    if (cert is None or cert == '') and fingerprint:
        x509_certificate_nodes = OneLogin_Saml2_XML.query(signature_node,
                                                          '//ds:Signature/ds:KeyInfo/ds:X509Data/ds:X509Certificate')
        if len(x509_certificate_nodes) > 0:
            x509_certificate_node = x509_certificate_nodes[0]
            x509_cert_value = OneLogin_Saml2_XML.element_text(x509_certificate_node)
            x509_cert_value_formatted = OneLogin_Saml2_Utils.format_cert(x509_cert_value)
            x509_fingerprint_value = OneLogin_Saml2_Utils.calculate_x509_fingerprint(x509_cert_value_formatted,
                                                                                     fingerprintalg)
            if fingerprint == x509_fingerprint_value:
                cert = x509_cert_value_formatted
            else:
                raise FingerPrintException(f'Expected certificate fingerprint {fingerprint} but got {x509_fingerprint_value}')

    if cert is None or cert == '':
        raise OneLogin_Saml2_Error(
            'Could not validate node signature: No certificate provided.',
            OneLogin_Saml2_Error.CERT_NOT_FOUND
        )

    if validatecert:
        manager = xmlsec.KeysManager()
        manager.load_cert_from_memory(cert, xmlsec.KeyFormat.CERT_PEM, xmlsec.KeyDataType.TRUSTED)
        dsig_ctx = xmlsec.SignatureContext(manager)
    else:
        dsig_ctx = xmlsec.SignatureContext()
        dsig_ctx.key = xmlsec.Key.from_memory(cert, xmlsec.KeyFormat.CERT_PEM, None)

    dsig_ctx.set_enabled_key_data([xmlsec.KeyData.X509])

    try:
        dsig_ctx.verify(signature_node)
    except Exception as err:
        raise OneLogin_Saml2_ValidationError(
            'Signature validation failed. %s',
            OneLogin_Saml2_ValidationError.INVALID_SIGNATURE,
            str(err)
        )

    return True


OneLogin_Saml2_Utils.validate_node_sign = validate_node_sign


def init_saml_auth(req, entity_id: str) -> OneLogin_Saml2_Auth:
    idp_metadata_xml = get_haka_metadata()
    idp_data = OneLogin_Saml2_IdPMetadataParser.parse(idp_metadata_xml, entity_id=entity_id)
    if 'idp' not in idp_data:
        raise RouteException(f'IdP not found from Haka metadata: {entity_id}')
    try:
        if not OneLogin_Saml2_Utils.validate_metadata_sign(
                idp_metadata_xml,
                validatecert=False,
                fingerprint=app.config['HAKA_METADATA_FINGERPRINT'],
                raise_exceptions=True,
        ):
            raise RouteException('Failed to validate Haka metadata')
    except (FingerPrintException, OneLogin_Saml2_ValidationError) as e:
        raise RouteException(f'Failed to validate Haka metadata: {e}')
    saml_path = app.config['SAML_PATH']
    osett = OneLogin_Saml2_Settings(custom_base_path=saml_path, sp_validation_only=True)
    sp = osett.get_sp_data()

    # merge_settings has incorrect type annotations
    # noinspection PyTypeChecker
    settings = OneLogin_Saml2_IdPMetadataParser.merge_settings({'sp': sp}, idp_data)
    auth = OneLogin_Saml2_Auth(req, settings, custom_base_path=saml_path)
    return auth


def get_haka_metadata() -> str:
    idp_metadata_xml = OneLogin_Saml2_IdPMetadataParser.get_metadata(app.config['HAKA_METADATA_URL'])
    return idp_metadata_xml


def prepare_flask_request(r: Request):
    url_data = urlparse(r.url)
    return {
        'https': 'on' if r.scheme == 'https' else 'off',
        'http_host': r.host,
        'server_port': url_data.port,
        'script_name': r.path,
        'get_data': r.args.copy(),
        'post_data': r.form.copy()
    }


@dataclass
class SSOData:
    return_to: str
    entityID: str


def prepare_and_init(entity_id: str) -> OneLogin_Saml2_Auth:
    req = prepare_flask_request(request)
    auth = init_saml_auth(req, entity_id)
    return auth


@dataclass
class TimRequestedAttributes:
    saml_auth: OneLogin_Saml2_Auth
    friendly_name_map: Dict = field(init=False)

    def __post_init__(self):
        self.friendly_name_map = {}
        settings: OneLogin_Saml2_Settings = self.saml_auth.get_settings()
        for ra in settings.get_sp_data()['attributeConsumingService']['requestedAttributes']:
            self.friendly_name_map[ra['friendlyName']] = ra['name']

    def get_attribute_by_friendly_name(self, name: str):
        return self.saml_auth.get_attribute(self.friendly_name_map[name])[0]

    @property
    def cn(self):
        return self.get_attribute_by_friendly_name('cn')

    @property
    def mail(self):
        return self.get_attribute_by_friendly_name('mail')

    @property
    def sn(self):
        return self.get_attribute_by_friendly_name('sn')

    @property
    def display_name(self):
        return self.get_attribute_by_friendly_name('displayName')

    @property
    def edu_person_principal_name(self):
        return self.get_attribute_by_friendly_name('eduPersonPrincipalName')

    @property
    def given_name(self):
        return self.get_attribute_by_friendly_name('givenName')

    @property
    def preferred_language(self):
        return self.get_attribute_by_friendly_name('preferredLanguage')

    def to_json(self):
        return {
            'cn': self.cn,
            'displayName': self.display_name,
            'eduPersonPrincipalName': self.edu_person_principal_name,
            'givenName': self.given_name,
            'mail': self.mail,
            'preferredLanguage': self.preferred_language,
            'sn': self.sn,
        }


@saml.route('/sso')
@use_model(SSOData)
def sso(m: SSOData):
    auth = prepare_and_init(m.entityID)
    session['entityID'] = m.entityID
    login_url = auth.login(return_to=m.return_to)
    session['requestID'] = auth.get_last_request_id()
    return redirect(login_url)


@csrf.exempt
@saml.route('/acs', methods=['post'])
def acs():
    auth = prepare_and_init(session['entityID'])
    request_id = session.get('requestID')
    if not request_id:
        raise RouteException('requestID missing from session')

    auth.process_response(request_id=request_id)
    errors = auth.get_errors()
    if not auth.is_authenticated():
        raise RouteException('Was not authenticated')
    if not errors:
        session.pop('requestID', None)
        self_url = request.url
        rs = request.form.get('RelayState')
        if rs and self_url != rs:
            return redirect(auth.redirect_to(rs))
        timattrs = TimRequestedAttributes(auth)
        return json_response(timattrs)
    else:
        return json_response({'errors': errors})


@saml.route('')
def get_metadata():
    saml_path = app.config['SAML_PATH']
    settings = OneLogin_Saml2_Settings(custom_base_path=saml_path, sp_validation_only=True)
    metadata = settings.get_sp_metadata()
    errors = settings.validate_metadata(metadata)

    if len(errors) == 0:
        resp = make_response(metadata, 200)
        resp.headers['Content-Type'] = 'text/xml'
    else:
        resp = make_response(', '.join(errors), 400)
    return resp


@saml.route('/feed')
def get_idps():
    idp_metadata_xml = get_haka_metadata()
    root = etree.fromstring(idp_metadata_xml)
    nsmap = copy(root.nsmap)
    rootns = nsmap.pop(None)
    nsmap['xhtml'] = rootns
    select_idps = CSSSelector(
        'xhtml|IDPSSODescriptor',
        namespaces=nsmap,
    )
    select_displaynames = CSSSelector(
        'mdui|DisplayName',
        namespaces=nsmap,
    )
    feed = []
    for idp in select_idps(root):
        names = []
        for n in select_displaynames(idp):
            names.append({
                'value': n.text,
                'lang': n.attrib['{http://www.w3.org/XML/1998/namespace}lang'],
            })
        feed.append({
            'entityID': idp.getparent().attrib['entityID'],
            'DisplayNames': names,
        })
    return json_response(feed)
