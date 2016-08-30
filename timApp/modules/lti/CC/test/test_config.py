"""
Configuration parameters for test suites
"""

config = {
    'credentials': {'consumer_key': '__consumer_key__', 'consumer_secret': '__consumer_secret__'},
    'lti_params': {'lti_message_type': 'basic-lti-launch-request', 'lti_version': 'LTI-1p0'},
    #'any_url': 'http://timstack.it.jyu.fi:5900/',
    'any_url': 'http://localhost:8080/',
    'moodle_url': 'http://timstack.it.jyu.fi:50002/',
    'moodle_params': {
        'lti_message_type': 'basic-lti-launch-request',
        'lti_version': 'LTI-1p0',
        'tool_consumer_instance_guid': 'localhost:5000',
        'tool_consumer_instance_description': 'Testing',
        'resource_link_id': '120988f929-274612',
        'context_id': '456434513',
        'context_title': "Custom LTI Consumer",
        'context_type': 'CourseSection',
        'context_label': 'SI182',
        'roles': 'Student',
        'user_id': 'joonas',
        'lis_person_sourced_id': 'school.edu:user',
        'lis_person_name_full': 'Joonas Keppo',
        'lis_person_contact_email_primary': 'joonas.s.keppo@student.jyu.fi',
        'launch_presentation_document_target': 'iframe'
    },
    'any_params': {
        'lti_message_type': 'basic-lti-launch-request',
        'lti_version': 'LTI-1p0'
    }

}

lti_xml_request = """<?xml version = "1.0" encoding = "UTF-8"?>
<imsx_POXEnvelopeRequest xmlns = "http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0">
  <imsx_POXHeader>
    <imsx_POXRequestHeaderInfo>
    <imsx_version>V1.0</imsx_version>
    <imsx_messageIdentifier>999999123</imsx_messageIdentifier>
    </imsx_POXRequestHeaderInfo>
  </imsx_POXHeader>
  <imsx_POXBody>
    <replaceResultRequest>
      <resultRecord>
        <sourcedGUID>
          <sourcedId>TestSourcedid</sourcedId>
        </sourcedGUID>
        <result>
          <resultScore>
            <language>en</language>
            <textString>0.42</textString>
          </resultScore>
        </result>
      </resultRecord>
    </replaceResultRequest>
  </imsx_POXBody>
</imsx_POXEnvelopeRequest>"""
