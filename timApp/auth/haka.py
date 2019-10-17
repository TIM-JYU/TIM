from typing import Dict
from urllib.parse import urlparse

import requests
from dataclasses import dataclass, field
from flask import request, redirect, session, make_response, Blueprint, Request
from onelogin.saml2.auth import OneLogin_Saml2_Auth
from onelogin.saml2.idp_metadata_parser import OneLogin_Saml2_IdPMetadataParser
from onelogin.saml2.settings import OneLogin_Saml2_Settings
from onelogin.saml2.utils import OneLogin_Saml2_Utils

from timApp.tim_app import app, csrf
from timApp.util.flask.requesthelper import use_model, RouteException
from timApp.util.flask.responsehelper import json_response, ok_response

haka = Blueprint('haka',
                 __name__,
                 url_prefix='/saml')


def init_saml_auth(req, entity_id: str):
    idp_metadata_xml = OneLogin_Saml2_IdPMetadataParser.get_metadata(app.config['HAKA_METADATA_URL'])
    idp_data = OneLogin_Saml2_IdPMetadataParser.parse(idp_metadata_xml, entity_id=entity_id)
    if 'idp' not in idp_data:
        raise RouteException(f'IdP not found from Haka metadata: {entity_id}')
    saml_path = app.config['SAML_PATH']
    cert = requests.get(app.config['HAKA_METADATA_CERT_URL']).text
    if not OneLogin_Saml2_Utils.validate_metadata_sign(idp_metadata_xml, cert=cert, validatecert=True):
        raise RouteException('Failed to validate Haka metadata')
    osett = OneLogin_Saml2_Settings(custom_base_path=saml_path, sp_validation_only=True)
    sp = osett.get_sp_data()

    # merge_settings has incorrect type annotations
    # noinspection PyTypeChecker
    settings = OneLogin_Saml2_IdPMetadataParser.merge_settings({'sp': sp}, idp_data)
    auth = OneLogin_Saml2_Auth(req, settings, custom_base_path=saml_path)
    return auth


def prepare_flask_request(r: Request):
    # If server is behind proxys or balancers use the HTTP_X_FORWARDED fields
    url_data = urlparse(r.url)
    return {
        'https': 'on' if r.scheme == 'https' else 'off',
        'http_host': r.host,
        'server_port': url_data.port,
        'script_name': r.path,
        'get_data': r.args.copy(),
        # Uncomment if using ADFS as IdP, https://github.com/onelogin/python-saml/pull/144
        # 'lowercase_urlencoding': True,
        'post_data': r.form.copy()
    }


@dataclass
class SSOData:
    target: str
    entityID: str


@haka.route('/sso')
@use_model(SSOData)
def sso(m: SSOData):
    auth = prepare_and_init(m.entityID)
    session['entityID'] = m.entityID
    return redirect(auth.login())


def prepare_and_init(entity_id: str):
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


@csrf.exempt
@haka.route('/acs', methods=['post'])
def acs():
    auth = prepare_and_init(session['entityID'])
    request_id = session.get('AuthNRequestID')

    auth.process_response(request_id=request_id)
    errors = auth.get_errors()
    # not_auth_warn = not auth.is_authenticated()
    if not errors:
        session.pop('AuthNRequestID', None)
        # session['samlNameId'] = auth.get_nameid()
        # session['samlNameIdFormat'] = auth.get_nameid_format()
        # session['samlNameIdNameQualifier'] = auth.get_nameid_nq()
        # session['samlNameIdSPNameQualifier'] = auth.get_nameid_spnq()
        # session['samlSessionIndex'] = auth.get_session_index()
        self_url = request.url
        # if 'RelayState' in request.form and self_url != request.form['RelayState']:
        #     return redirect(auth.redirect_to(request.form['RelayState']))
        timattrs = TimRequestedAttributes(auth)
        return json_response(timattrs)
    else:
        return json_response({'errors': errors})


@csrf.exempt
@haka.route('/sls', methods=['GET', 'POST'])
def index():
    auth = prepare_and_init(session['entityID'])
    errors = []
    not_auth_warn = False
    success_slo = False
    attributes = False
    paint_logout = False

    if 'slo' in request.args:
        name_id = session.get('samlNameId')
        session_index = session.get('samlSessionIndex')
        name_id_format = session.get('samlNameIdFormat')
        name_id_nq = session.get('samlNameIdNameQualifier')
        name_id_spnq = session.get('samlNameIdSPNameQualifier')

        return redirect(
            auth.logout(name_id=name_id, session_index=session_index, nq=name_id_nq, name_id_format=name_id_format,
                        spnq=name_id_spnq))
    elif 'acs' in request.args:
        pass
    elif 'sls' in request.args:
        request_id = session.get('LogoutRequestID')
        dscb = lambda: session.clear()
        url = auth.process_slo(request_id=request_id, delete_session_cb=dscb)
        errors = auth.get_errors()
        if len(errors) == 0:
            if url is not None:
                return redirect(url)
            else:
                success_slo = True

    return ok_response()


@haka.route('')
def get_metadata():
    req = prepare_flask_request(request)
    auth = init_saml_auth(req, 'https://testidp.funet.fi/idp/shibboleth')
    settings = auth.get_settings()
    metadata = settings.get_sp_metadata()
    errors = settings.validate_metadata(metadata)

    if len(errors) == 0:
        resp = make_response(metadata, 200)
        resp.headers['Content-Type'] = 'text/xml'
    else:
        resp = make_response(', '.join(errors), 500)
    return resp
