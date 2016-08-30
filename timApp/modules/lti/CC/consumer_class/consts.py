"""
consts.py

Define fixed requirements and other constant values
to be used in the Consumer class.
"""

import copy

# Additional parameters for Moodle-based LTI Providers
__MOODLE_PARAMS = ['roles', 'user_id', 'resource_link_id', 'tool_consumer_instance_guid', \
                'lis_person_name_full', 'context_id', \
                'context_title', 'launch_presentation_document_target']

# Consts defined below
ACCEPTED_VALUES = {'lti_message_type': ['basic-lti-launch-request'],
                   'lti_version': ['LTI-1p0']}

HEADERS = {'Content-Type': 'application/x-www-form-urlencoded'}

REQUIRED_ATTRS = {'form': ['target', 'name', 'id'],
                  'iframe': ['id', 'name', 'src']}

CONS_ATTRS = ['credentials', 'lti_params', 'tp_url', 'profile']

TP_PROFILES = {
    'any': {'credentials': ['consumer_key', 'consumer_secret'],
            'lti_params': ['lti_message_type', 'lti_version']},
}
TP_PROFILES['moodle'] = copy.deepcopy(TP_PROFILES['any'])
TP_PROFILES['moodle']['lti_params'].extend(__MOODLE_PARAMS)
