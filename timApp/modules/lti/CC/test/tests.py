"""
tests.py

All unit tests for Consumer class
"""

import unittest
from bs4 import BeautifulSoup
from consumer_class.consumer import (Consumer, Validation)
from consumer_class.utils import isValidXML
from test.test_config import config


test_consumer = Consumer(credentials=config['credentials'], lti_params=config['any_params'], \
                         profile='any', tp_url=config['any_url'])


class ConsumerUnitTests(unittest.TestCase):

    def test_post(self):
        """POST returns correct status code (200)."""
        self.assertEqual(test_consumer.post().status_code, 200)

    def test_get(self):
        """GET returns correct status code (200)."""
        test_consumer.post()
        self.assertEqual(test_consumer.get().status_code, 200)

    def test_response_text(self):
        """Same consumer object should always give back same response text."""
        test_consumer.authorize()
        r1 = test_consumer.post()
        r2 = test_consumer.post()
        self.assertEqual(r1.text, r2.text)

    def test_credentials(self):
        """Invalid credentials should not pass validation."""
        # credentials is assumed to be valid here
        bad_creds = config['credentials'].copy()
        bad_creds['consumer_key'] = '__incorrect_key__'
        url = config['any_url']
        lti = config['lti_params']
        test_consumer2 = Consumer(credentials=bad_creds, tp_url=url, lti_params=lti)
        # TODO: assertions?

    def test_make_form(self):
        """Ensure that at least semi-valid HTML is being produced for <form>."""
        test_html = test_consumer.make(element='form')
        self.assertEqual(bool(BeautifulSoup(test_html, "html.parser").find()), True)

    def test_make_iframe(self):
        """Ensure that at least semi-valid HTML is being produced for <iframe>."""
        attrs = {
            'target': 'dummyTarget',
            'id': 'dummyId',
            'unnecessary': 'necessary',
            'name': 'my-name',
            'src': 'about:blank'
        }
        test_html = test_consumer.make(element='iframe', attrs=attrs)
        self.assertEqual(bool(BeautifulSoup(test_html, "html.parser").find()), True)

    def test_profile(self):
        """Ensure that lti_params are consistent with the specified LTI Provider profile."""
        # shouldn't fail
        Validation.profile(test_consumer)
        # should fail
        creds = config['credentials']
        moodle = config['moodle_params']
        moodle_consumer = Consumer(credentials=creds, lti_params=moodle)
        self.assertRaises(KeyError, Validation.profile(moodle_consumer, profile='moodle'))
        
    def test_xml_content_validation(self):
        """Is generated XML valid?"""
        schema = 'https://www.imsglobal.org/lti/media/ltiv1p1/OMSv1p0_LTIv1p1Profile_SyncXSD_v1p0.xsd'
        xml = '''<?xml version = "1.0" encoding = "UTF-8"?>
        <imsx_POXEnvelopeRequest xmlns = "http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0">  
          <imsx_POXHeader>    
            <imsx_POXRequestHeaderInfo>     
              <imsx_version>V1.0</imsx_version>
              <imsx_messageIdentifier>999999123</imsx_messageIdentifier>    
            </imsx_POXRequestHeaderInfo>  
          </imsx_POXHeader>  
          <imsx_POXBody>    
            <readResultRequest>     
              <resultRecord>
                <sourcedGUID>
                  <sourcedId>3124567</sourcedId>
                </sourcedGUID>
              </resultRecord>   
            </readResultRequest>  
          </imsx_POXBody> 
        </imsx_POXEnvelopeRequest>'''
        self.assertEqual(isValidXML(xml=xml, schema=schema), True)

    def test_invalid_xml_url(self):
        """Invalid XML URL should fail validation."""
        schema = 'https://www.imsglobal.org/lti/media/ltiv1p1/OMSv1p0_LTIv1p1Profile_SyncXSD_v1p0.xsd'
        url = 'https://raw.githubusercontent.com/CIDI/bulk-lti-tool/master/bulkLTI.xml'
        self.assertEqual(isValidXML(url=url, schema=schema), False)

    def test_make_form_cookies(self):
        """Make form that uses cookies from previous POST to TP."""
        cookies = test_consumer.post().cookies
        test_consumer.make(element='form', js=True, cookies=cookies)

   
if __name__ == '__main__':
    unittest.main()
