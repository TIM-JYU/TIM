#!/bin/env python3
# -*- coding: utf-8 -*-

"""
consumer.py

LTI Consumer class
"""


import time
import xmltodict
import requests
from urllib.parse import urlparse
import oauth2
from copy import deepcopy
from CC.consumer_class.consts import (ACCEPTED_VALUES, HEADERS, REQUIRED_ATTRS,
                                   CONS_ATTRS, TP_PROFILES)
from CC.consumer_class.utils import (InvalidConsumerError, ConsumerHTMLError,
                                  ConsumerXMLError)


class Validation:

    @staticmethod
    def params(obj):
        """Validate credentials and lti_params."""
        to_validate = {
            'credentials': obj.credentials,
            'lti_params': obj.lti_params
        }
        for req in TP_PROFILES[obj.profile]:
            for item in TP_PROFILES[obj.profile][req]:
                try:
                    val_item = to_validate[req][item]
                    if val_item in [None, '']:
                        err_msg = 'Empty value in %(req)s (%(item)s)'
                        err_dict = {
                            'req': req,
                            'item': item
                        }
                        raise InvalidConsumerError(err_msg % err_dict)
                    # Check specific value constraints
                    elif item in ACCEPTED_VALUES and val_item not in ACCEPTED_VALUES[item]:
                        accepted = ', '.join(ACCEPTED_VALUES[item])
                        err_dict = {
                            'req': req,
                            'item': item,
                            'acc': accepted
                        }
                        err_msg = 'Invalid value in %(req)s (%(item)s). Accepted value(s): %(acc)s.'
                        raise InvalidConsumerError(err_msg % err_dict)
                except KeyError:
                    raise InvalidConsumerError('Missing key-value pair in %s (%s)' % (req, item))

    @staticmethod
    def html(obj):
        """Validate the Tool Provider URL as much as possible."""
        url_error = InvalidConsumerError('Valid tp_url required!')
        try:
            parsed = urlparse(obj.tp_url)
            if not (bool(parsed.scheme) and bool(parsed.netloc)):
                raise url_error
        except AttributeError:
            raise url_error

    @staticmethod
    def profile(obj, profile=None): # TODO: detailed reporting?
        """
        Check current profile against required and recommended parameters.
        If profile is specified, check against profile or else check against self.profile
        """
        prof = profile or obj.profile
        for req in TP_PROFILES[prof]['lti_params']:
            if not req in obj.lti_params:
                raise InvalidConsumerError('Missing parameter "%s" for profile "%s"!' % (req, prof))


class Consumer:

    def __init__(self, **attrs):
        """Initialize a new Tool Consumer."""
        # Defaults
        self.profile = 'any'
        self.credentials = {
            'consumer_key': '__consumer_key__',
            'consumer_secret': '__lti_secret__'
        }
        self.lti_params = {
            'lti_message_type': ACCEPTED_VALUES['lti_message_type'][0],
            'lti_version': ACCEPTED_VALUES['lti_version'][0]
        }
        self.tp_url = ''

        # Set attributes and possibly overwrite defaults
        for arg in attrs.items():
            if arg[0] in CONS_ATTRS:
                setattr(self, arg[0], arg[1])
            else:
                raise AttributeError('Unknown attribute "' + arg[0] + '"!')

        # Make sure that args are valid
        Validation.params(self)

        # Internal ('private') variables
        self.__signed = {}
        self.__response = {}

    def authorize(self):
        """Authorize POST."""
        # Ensure valid entries
        Validation.profile(self)
        Validation.html(self)

        # Aggregate OAuth things here
        oauthing = {}

        # An aggregation of everything needed as launch parameters
        launch_data = deepcopy(self.lti_params)
        launch_data.update(self.credentials)

        # Using OAuth 1.0
        launch_data['oauth_version'] = '1.0'

        # Get current UNIX time for timestamp and generate nonce value
        launch_data['oauth_timestamp'] = str(int(time.time()))
        launch_data['oauth_nonce'] = oauth2.generate_nonce()

        # Create an OAuth Consumer
        oauthing['consumer'] = oauth2.Consumer(key=launch_data['consumer_key'],\
            secret=launch_data['consumer_secret'])
        launch_data['oauth_consumer_key'] = oauthing['consumer'].key

        # Use HMAC-SHA1 algorithm for signing
        oauthing['sig_method'] = oauth2.SignatureMethod_HMAC_SHA1()
        launch_data['oauth_signature_method'] = 'HMAC-SHA1'

        # "not used by LTI so should always have a value of about:blank"
        launch_data['oauth_callback'] = 'about:blank'

        # Create an OAuth POST request and sign it with HMAC-SHA1
        oauthing['request'] = oauth2.Request(method='POST', url=self.tp_url, parameters=launch_data)
        try:
            oauthing['request'].sign_request(oauthing['sig_method'], oauthing['consumer'], None)
        except AttributeError:
            raise InvalidConsumerError('Unknown issue with lti_params or credentials!')

        self.__signed = oauthing['request']

        # Wrong formatting -- UTF-8 encoding messes with <input> stuff
        self.__signed['oauth_body_hash'] = self.__signed['oauth_body_hash'].decode('utf-8')
        self.__signed['oauth_signature'] = self.__signed['oauth_signature'].decode('utf-8')

        return self.__signed

    def post(self, url=None):
        """Send POST request using signed launch data"""
        # If either not OAuth-signed or signature already used once
        if not bool(self.__signed) or self.__response:
            self.authorize()
        url = url or self.tp_url
        self.__response = requests.post(url, data=self.__signed, headers=HEADERS)
        self.__signed = {} # Must reset for next request
        return self.__response

    def get(self, url=None):
        """Send GET request using cookies from initial POST."""
        if not self.__response:
            return None
        url = url or self.tp_url
        return requests.get(url, cookies=self.__response.cookies, headers=HEADERS)

    def make_iframe(self, attrs={}):
        """
        Return a string representation of an HTML <iframe> element.
        Used with <form>, which can be generated with __form.
        """
        data = {
            'src': attrs.get('src') or 'about:blank',
            'name': attrs.get('name') or 'launchFrame',
            'id': attrs.get('id') or 'id_launchFrame'
        }
        iframe = "<iframe src='%(src)s' name='%(name)s' id='%(id)s'></iframe>"
        return iframe % data

    def make_form(self, attrs={}, js=True, hidden=True, target_iframe=False):
        """
        Return a string representation of an HTML <form> element with hidden <input> tags.
        This form can then be submitted via JavaScript.
        Can either create a form for the initial POST request (for getting session cookies)
        or alternatively create forms that use cookies from
        """
        # Sanity checks
        if not bool(self.tp_url):
            raise ConsumerHTMLError('Need to provide tp_url to create %s!' % (element))
        if not self.__signed:
            self.authorize()

        data = {
            'action': self.tp_url,
            'ctype': HEADERS['Content-Type'],
            'method': 'post',
            'target': attrs.get('target') or 'launchFrame',
            'name': attrs.get('name') or 'frmLaunch',
            'id': attrs.get('id') or 'id_frmLaunch'
        }

        form = """<form action='%(action)s' name='%(name)s'
                id='%(id)s' method='%(method)s' enctype='%(ctype)s' """
        if target_iframe:
            form += 'target="%(target)s" '
        if not hidden:
            form += 'style="display: flex; flex-direction: column;"'
        form += '>\n'

        if not hidden:
            form += '<input type="submit" value="Submit" style="height: 50px; font-size: 1em;">'
            form += '<br><br>'
        form = form % data

        # Add in hidden <input> fields
        for param in self.__signed:
            param_data = {
                'name': param,
                'value': str(self.__signed[param])
            }
            if hidden:
                form += '\t<input name="%(name)s" value="%(value)s" type="hidden">\n'
            else:
                form += '\t<label for="%(name)s">%(name)s</label>'
                form += '<input name="%(name)s" value="%(value)s"><br>\n'
            form = form % param_data
        form +=  '</form>\n'

        # Add optional script tag
        if js:
            form += """<script>
                            frm=document.getElementById('%(id)s');
                            frm.submit();
                        </script>"""
            form = form % data

        return form


class Outcomes:

    def __init__(self, xml):
        """Initialize an LTI Outcomes wrapper class."""
        self.xml = xml
        self.req_data = {}
        self.res_data = {}
        self.__types = ['replaceResult', 'deleteResult', 'readResult']

    def parse_request(self):
        """
        Parse incoming POST request's XML body (Outcome Request).
        This is used for communicating grades (float from 0 to 1, inclusive).

        Partly adapted from process_xml function in the ims_lti_py repo on Github
        """
        from re import search

        try:
            root = xmltodict.parse(self.xml)['imsx_POXEnvelopeRequest']
            msg_id = root['imsx_POXHeader']['imsx_POXRequestHeaderInfo']['imsx_messageIdentifier']
            self.req_data['message_identifier'] = str(msg_id)
        except:
            raise ConsumerXMLError('Failed to parse Outcomes Request XML body!')

        # Array of possible results in XML body
        results = list(map(lambda type: type + 'Request', self.__types))

        for result in results:
            try:
                res_val = root['imsx_POXBody'][result]
                src_id = res_val['resultRecord']['sourcedGUID']['sourcedId']
                # Look for 'Request' in `result`...
                end_idx = search('Request', result).start()
                # ... and trim it away
                self.req_data['type'] = result[:end_idx]
                self.req_data['lis_result_sourcedid'] = src_id
                if result == 'replaceResultRequest':
                    text_str = res_val['resultRecord']['result']['resultScore']['textString']
                    self.req_data['score'] = str(text_str)
            except:
                pass

        return self.req_data

    def make_response(self):
        """Generate XML based on the current configuration."""
        res_data = self.res_data or {}
        req_data = self.req_data or {}
        data = {'xmlns': 'http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0'}
        try:
            data['msg_ref'] = req_data['message_identifier']
            data['res_type'] = req_data['type'] + 'Response'
        except:
            raise KeyError('Missing request data. Make sure that parse_request is called prior to make_response!')

        data['code_major'] = res_data.get('code_major') or 'success'
        data['severity'] = res_data.get('severity') or 'status'
        data['desc'] = res_data.get('desc') or 'None'
        data['op_ref'] = res_data.get('op_ref') or 'None'
        data['lang'] = res_data.get('lang') or 'en'

        xml = """<?xml version='1.0' encoding='UTF-8'?>
                    <imsx_POXEnvelopeResponse xmlns='%(xmlns)s'>
                    <imsx_POXHeader>
                        <imsx_POXResponseHeaderInfo>
                        <imsx_version>V1.0</imsx_version>
                        <imsx_messageIdentifier>%(msg_ref)s</imsx_messageIdentifier>
                        <imsx_statusInfo>
                            <imsx_codeMajor>%(code_major)s</imsx_codeMajor>
                            <imsx_severity>%(severity)s</imsx_severity>
                            <imsx_description>%(desc)s</imsx_description>
                            <imsx_messageRefIdentifier>%(msg_ref)s</imsx_messageRefIdentifier>
                            <imsx_operationRefIdentifier>%(op_ref)s</imsx_operationRefIdentifier>
                        </imsx_statusInfo>
                        </imsx_POXResponseHeaderInfo>
                    </imsx_POXHeader>
                    <imsx_POXBody>\n"""

        if data['res_type'] in ['replaceResultResponse', 'deleteResultResponse']:
            xml += '      <%(res_type)s/>\n'
        elif data['res_type'] == 'readResultResponse':
            xml += """      <%(res_type)s>
                                <result>
                                <resultScore>
                                    <language>%(lang)s</language>
                                    <textString>%(score)s</textString>
                                </resultScore>
                                </result>
                            </%(res_type)s>\n"""
        else:
            raise ValueError('Unknown response type "(res_type)s"!' % data)

        xml = xml % data # Format string
        xml += """  </imsx_POXBody>
                    </imsx_POXEnvelopeResponse>"""

        return xml


    @staticmethod
    def handle(xml, callback=None):
        """
        Do Outcomes process from request to response.
        :return Outcomes Response XML string
        """

        def __validate_callback(cb_data):
            valid_keys = ['code_major', 'severity', 'desc', 'op_ref', 'lang']
            for key in cb_data:
                if key not in valid_keys:
                    raise KeyError('Invalid key "%s" in callback return!' % key)

        cb_data = {}
        oc = Outcomes(xml)
        # Parse incoming Outcomes Request XML body (sent from Tool Provider)
        req = oc.parse_request()
        if callback:
            # Call request callback (if one provided)
            oc.res_data = callback(req)
            if (type(oc.res_data) is not dict) and (oc.res_data is not None):
                raise TypeError("Request callback must either return a dict or nothing!")
            elif type(oc.res_data) is dict:
                __validate_callback(oc.res_data)
        # Return Outcome Response XML body (sent to Tool Provider)
        return oc.make_response()
