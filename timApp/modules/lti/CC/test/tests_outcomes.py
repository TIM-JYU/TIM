"""
tests_outcomes.py

Outcomes-related unit testing
"""

import unittest
from consumer_class.consumer import Consumer
from consumer_class.utils import isValidXML # TODO: make tests with this!
from test.test_config import lti_xml_request as test_xml

test_consumer = Consumer()

class OutcomesUnitTests(unittest.TestCase):

    def test_outcomes_valid_response(self):
        pass # TODO

    def test_callback_none(self):
        def no_return(req):
            return
        test_consumer.outcomes(test_xml, req_callback=no_return)
        pass

    def test_callback_mutate_data(self):
        def data_mutator(req):
            return {'code_major': 'unsuccessful'}
        res1 = test_consumer.outcomes(test_xml)
        res2 = test_consumer.outcomes(test_xml, req_callback=data_mutator)
        self.assertEqual(res1 == res2, False)

if __name__ == '__main__':
    unittest.main()
