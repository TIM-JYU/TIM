import unittest

from timApp.util.utils import title_to_id


class TitleToIdTest(unittest.TestCase):
    def test_title_to_id(self):
        self.assertEqual(title_to_id('aa  ää öö åå 44 [] xx $! AA - b _ c / d é'), 'aa-ää-öö-åå-44-xx-aa---b-_-c-d-é')
        self.assertEqual(title_to_id('1234'), 'section')
        self.assertEqual(title_to_id('1234:'), 'section')
        self.assertEqual(title_to_id('123 123'), 'section')
