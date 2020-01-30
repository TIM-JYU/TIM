from unittest import TestCase

from timApp.user.personaluniquecode import SchacPersonalUniqueCode


class TestUniqueCode(TestCase):

    def test_parse(self):
        p = SchacPersonalUniqueCode.parse('urn:schac:personalUniqueCode:int:studentID:jyu.fi:123456')
        self.assertEqual('123456', p.code)
        self.assertEqual('studentID', p.codetype)
        self.assertEqual('jyu.fi', p.org)

        p = SchacPersonalUniqueCode.parse('urn:mace:terena.org:schac:personalUniqueCode:int:studentID:aalto.fi:1234')
        self.assertEqual('1234', p.code)
        self.assertEqual('studentID', p.codetype)
        self.assertEqual('aalto.fi', p.org)

        p = SchacPersonalUniqueCode.parse('urn:terena.org:schac:personalUniqueCode:int:studentID:aalto.fi:1234')
        self.assertIsNone(p)

        # Make sure the regex is properly escaped so that "." does not accidentally match any character.
        p = SchacPersonalUniqueCode.parse('urn:mace:terenaxorg:schac:personalUniqueCode:int:studentID:aalto.fi:1234')
        self.assertIsNone(p)
