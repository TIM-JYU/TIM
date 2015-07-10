import unittest
from documentmodel.attributeparser import AttributeParser


class AttributeParserTest(unittest.TestCase):
    def test_random(self):
        self.check_valid('{}', {})
        self.check_valid('{#asd}', {'taskId': 'asd'})
        self.check_valid('{#asd .someClass .someClass2}', {'taskId': 'asd',
                                                           'classes': ['someClass', 'someClass2']})
        self.check_valid('{#asd .someClass somekey=someval emptykey= .someClass2}',
                         {
                             'taskId': 'asd',
                             'classes': ['someClass', 'someClass2'],
                             'somekey': 'someval',
                             'emptykey': ''
                         })

    def test_keyvalues(self):
        self.check_valid('{somekey=someval}', {'somekey': 'someval'})
        self.check_valid('{somekey=}', {'somekey': ''})
        self.check_valid('{somekey=""}', {'somekey': ''})
        self.check_valid('{somekey=a""}', {'somekey': 'a""'})
        self.check_valid('{somekey="someval"}', {'somekey': 'someval'})
        self.check_valid('{somekey="someval with spaces"}', {'somekey': 'someval with spaces'})
        self.check_valid(r'{somekey="\""}', {'somekey': '"'})
        self.check_valid(r'{somekey=\}', {'somekey': '\\'})
        self.check_valid(r'{somekey=\"}', {'somekey': '\\"'})

    def test_whitespace(self):
        self.check_valid('  { #asd  }   ', {'taskId': 'asd'}, 2)
        self.check_valid('#Header { #asd  }   ', {'taskId': 'asd'}, 8)
        self.check_valid('``` {#asd plugin=csPlugin}   ',
                         {
                             'taskId': 'asd',
                             'plugin': 'csPlugin'
                         }, 4)

    def check_valid(self, string, expected, expected_index=0):
        ap = AttributeParser()
        attrs, index = ap.set_str(string).get_attributes()
        self.assertDictEqual(expected, attrs)
        self.assertEqual(expected_index, index)

    def check_invalid(self, string):
        ap = AttributeParser()
        attrs, index = ap.set_str(string).get_attributes()
        self.assertDictEqual({}, attrs)
        self.assertEqual(None, index)

    def test_broken(self):
        self.check_invalid('  { #asd     ')
        self.check_invalid('  { #asd')
        self.check_invalid(r'\{ #asd}')
        self.check_invalid('   #asd     }')
        self.check_invalid('{somekey="}')
        self.check_invalid('{somekey="""}')
        self.check_invalid('{.}')
        self.check_invalid('{=}')
        self.check_invalid('{#}')
        self.check_invalid('{#task1 #task2}')  # Only 1 task id allowed

if __name__ == '__main__':
    unittest.main()
