import unittest
from unittest import TestCase

from timApp.markdown.markdownconverter import end_value, shuffle


class TestEndValue(TestCase):
    def test_empty(self):
        self.assertEqual("", end_value(""), "Not same in empty")

    def test_empty_def(self):
        self.assertEqual("2", end_value("", "2"), "Not same in empty def")

    def test_one_number(self):
        s = "5"
        self.assertEqual("5", end_value(s, "2"), "Not same in one number " + s)

    def test_one_char(self):
        s = "a"
        self.assertEqual("3", end_value(s, "3"), "Not same in one char " + s)

    def test_two_number(self):
        s = "67"
        self.assertEqual("67", end_value(s, "3"), "Not same in two number " + s)

    def test_number_char(self):
        s = "1a"
        self.assertEqual("4", end_value(s, "4"), "Not same in one number char " + s)

    def test_char_number(self):
        s = "b5"
        self.assertEqual("5", end_value(s, "4"), "Not same in one char number " + s)

    def test_many_number(self):
        s = "8765"
        self.assertEqual("8765", end_value(s, "4"), "Not same in many number " + s)

    def test_string_many_number(self):
        s = "cat1357"
        self.assertEqual(
            "1357", end_value(s, "4"), "Not same in string many number " + s
        )

    def test_string_value_string_value(self):
        s = "cat1357a654"
        self.assertEqual(
            "654",
            end_value(s, "4"),
            "Not same in string value string value " + s,
        )

    def test_int_value(self):
        s = 123
        self.assertEqual(
            "123",
            end_value(s, "4"),
            "Not same in int " + str(s),
        )

    def test_int_defvalue(self):
        s = "8765a"
        self.assertEqual("9", end_value(s, 9), "Not in int defvalue " + s)


class TestShuffle(unittest.TestCase):
    def test_shuffle(self):
        self.assertEqual(shuffle([1, 2, 3], 1), [2, 3, 1])
        self.assertEqual(shuffle([1, 2, 3], 10), [2, 1, 3])

        self.assertEqual(shuffle("abc", 1), ["b", "c", "a"])

        self.assertEqual(shuffle({1, 2, 3}, 1), [2, 3, 1])

        self.assertEqual(shuffle({"a": 1, "b": 2, "c": 3}, 1), ["b", "c", "a"])
