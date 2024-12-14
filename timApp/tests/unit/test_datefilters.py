from unittest import TestCase
from datetime import date, datetime, timezone, timedelta

from timApp.markdown.markdownconverter import (
    week_to_date,
    week_to_text,
    month_to_week,
    fmt_date,
    str_to_date,
    preinc,
    postinc,
)


class TestWeekToDate(TestCase):
    def test_w2date_normal(self):
        r = str(week_to_date(2, 1, 2020))
        e = "2020-01-06"
        self.assertEqual(e, r, "Not same in normal case")

    def test_w2date_sunday(self):
        r = str(week_to_date(2, 7, 2020))
        e = "2020-01-12"
        self.assertEqual(e, r, "Not same in sunday case")

    def test_w2date_default(self):
        r = str(week_to_date(0))
        t = date.today()
        w = t.isocalendar()[1]

        e = str(date.fromisocalendar(t.year, w, 1))
        self.assertEqual(e, r, "Not same in default case")

    def test_w2date_format(self):
        r = str(week_to_date(2, 1, 2020, ""))
        e = "6.1"
        self.assertEqual(e, r, "Not same in format case")


class TestMonthToWeek(TestCase):
    def test_m2w_normal(self):
        r = month_to_week(3, 2, 2020)
        e = 10
        self.assertEqual(e, r, "Not same in normal case")


class TestFormatDate(TestCase):
    def test_fmt_date_normal(self):
        d = datetime(2020, 2, 5)
        r = fmt_date(d, "%d.%m")
        e = "05.02"
        self.assertEqual(e, r, "Not same in normal case")

    def test_fmt_date_default(self):
        d = datetime(2020, 2, 5)
        r = fmt_date(d)
        e = "5.2"
        self.assertEqual(e, r, "Not same in default case")

    def test_fmt_date_str(self):
        d = "2024-02-05 15+02"
        r = fmt_date(d, "%d.%m.%Y")
        e = "05.02.2024"
        self.assertEqual(e, r, "Not same in str case")


class TestStrToDate(TestCase):
    def test_str2date_normal(self):
        r = str_to_date("5.2.2020")
        e = datetime(2020, 2, 5)
        self.assertEqual(e, r, "Not same in normal case")

    def test_str2date_one(self):
        r = str_to_date("5")
        e = datetime(date.today().year, date.today().month, 5)
        self.assertEqual(e, r, "Not same in normal case")

    def test_str2date_short(self):
        r = str_to_date("5.2")
        e = datetime(date.today().year, 2, 5)
        self.assertEqual(e, r, "Not same in short case")

    def test_str2date_ISO_date(self):
        r = str_to_date("2024-12-13")
        e = datetime(2024, 12, 13)
        self.assertEqual(e, r, "Not same in ISO_date case")

    def test_str2date_ISO(self):
        r = str_to_date("2024-12-13 15")
        e = datetime(2024, 12, 13, 15, 0)
        self.assertEqual(e, r, "Not same in ISO case")

    def test_str2date_ISO_z(self):
        r = str_to_date("2024-12-13 15+02")
        e = datetime(2024, 12, 13, 15, 0, tzinfo=timezone(timedelta(hours=2)))
        self.assertEqual(e, r, "Not same in ISO z case")

    def test_str2date_ISO_zm(self):
        r = str_to_date("2024-12-13 15:30+03")
        e = datetime(2024, 12, 13, 15, 30, tzinfo=timezone(timedelta(hours=3)))
        self.assertEqual(e, r, "Not same in ISO zm case")


class TestInc(TestCase):
    def test_pre_inc1(self):
        t = [3]
        r = preinc(t)
        self.assertEqual(4, r, "Not same in r inc 1 case")
        self.assertEqual(4, t[0], "Not same t in inc 1 case")

    def test_pre_inc2(self):
        t = [3]
        r = preinc(t, 2)
        self.assertEqual(5, r, "Not same in r inc 2 case")
        self.assertEqual(5, t[0], "Not same t in inc 2 case")

    def test_post_inc1(self):
        t = [3]
        r = postinc(t)
        self.assertEqual(3, r, "Not same in r inc 1 case")
        self.assertEqual(4, t[0], "Not same t in inc 1 case")

    def test_post_inc2(self):
        t = [3]
        r = postinc(t, 2)
        self.assertEqual(3, r, "Not same in r inc 2 case")
        self.assertEqual(5, t[0], "Not same t in inc 2 case")


class TestWeekToText(TestCase):
    def test_w2text_normal(self):
        r = week_to_text(2, 2020)
        e = "ma 6.1|ti 7.1|ke 8.1|to 9.1|pe 10.1|"
        self.assertEqual(e, r, "Not same in normal case")

    def test_w2text_shorter(self):
        r = week_to_text(2, 2020, " %d ")
        e = "ma 06 ti 07 ke 08 to 09 pe 10 "
        self.assertEqual(e, r, "Not same in %d case case")

    def test_w2text_just3(self):
        r = week_to_text(2, 2020, " %d ", "wed|fri|sat|", 3)
        e = "wed 08 fri 09 sat 10 "
        self.assertEqual(e, r, "Not same in just 3 case")

    def test_w2text_just_dates(self):
        r = week_to_text(2, 2020, "%d ", "|||", 3)
        e = "08 09 10 "
        self.assertEqual(e, r, "Not same in just dates case")

    def test_w2text_end_of_year(self):
        r = week_to_text(53, 2020, days="ma|ti|ke|to|pe|la|su|")
        e = "ma 28.12|ti 29.12|ke 30.12|to 31.12|pe 1.1|la 2.1|su 3.1|"
        self.assertEqual(e, r, "Not same in end of year case")

    def test_w2text_end_of_year2(self):
        r = week_to_text(53, 2020, "|%d ", "|ma|ti|ke|to|pe|la|su")
        e = "|28 ma|29 ti|30 ke|31 to|01 pe|02 la|03 su"
        self.assertEqual(e, r, "Not same in end of year2 case")

    def test_w2text_too_many(self):
        r = week_to_text(53, 2020, " %d1|", "pe|la|su|ma|ti|", 5)
        e = "pe 1|la 2|su 3|"
        self.assertEqual(e, r, "Not same in too many case")

    def test_w2text_too_many2(self):
        r = week_to_text(53, 2020, "|%d ", "|pe|la|su|ma|ti|", 5)
        e = "|01 pe|02 la|03 su"
        self.assertEqual(e, r, "Not same in too many2 case")
