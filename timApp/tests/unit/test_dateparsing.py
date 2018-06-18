import unittest
from datetime import timezone

from dateutil.tz import tzlocal

from timApp.util.utils import getdatetime


class DateParseTest(unittest.TestCase):
    def test_parse_human_readable(self):
        d = getdatetime("17.10.2016 11:05")
        self.assertEqual(d.month, 10)
        self.assertEqual(d.day, 17)
        self.assertEqual(d.year, 2016)
        self.assertEqual(d.hour, 11)
        self.assertEqual(d.minute, 5)
        self.assertEqual(d.tzinfo, timezone.utc)

    def test_parse_human_readable_ambiguous(self):
        d = getdatetime("11.10.2016 11:05")
        self.assertEqual(d.month, 10)
        self.assertEqual(d.day, 11)
        self.assertEqual(d.year, 2016)
        self.assertEqual(d.hour, 11)
        self.assertEqual(d.minute, 5)
        self.assertEqual(d.tzinfo, timezone.utc)

    def test_parse_isodate(self):
        d = getdatetime("2017-09-11T11:08:00.000Z")
        self.assertEqual(d.month, 9)
        self.assertEqual(d.day, 11)
        self.assertEqual(d.year, 2017)
        self.assertEqual(d.hour, 11)
        self.assertEqual(d.minute, 8)
        self.assertEqual(d.tzinfo, tzlocal())

    def test_parse_custom(self):
        d = getdatetime("2017-04-05 11:15Z")
        self.assertEqual(d.month, 4)
        self.assertEqual(d.day, 5)
        self.assertEqual(d.year, 2017)
        self.assertEqual(d.hour, 11)
        self.assertEqual(d.minute, 15)
        self.assertEqual(d.tzinfo, tzlocal())
