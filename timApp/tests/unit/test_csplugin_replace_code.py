from unittest import TestCase
from timApp.modules.cs.cs_utils import replace_code


class TestReplaceCode(TestCase):
    def test_empty_rules(self):
        self.assertEqual("cat", replace_code(None, "cat"), "Not same in empty")

    def test_simple_rule(self):
        rules = [{"replace": "at", "by": "cc"}]
        self.assertEqual("ccc", replace_code(rules, "cat"), "Not same in at => cc")

    def test_simple_rule_many(self):
        rules = [{"replace": "at", "by": "cc"}]
        self.assertEqual(
            "ccc ccc", replace_code(rules, "cat cat"), "Not same in at => cc 2x"
        )

    def test_regexp_rule(self):
        rules = [{"replace": "at.*", "by": "cc"}]
        self.assertEqual(
            "ccc", replace_code(rules, "cat cat"), "Not same in at => cc reg"
        )

    def test_many_lines_remove(self):
        rules = [{"replace": "at.*", "by": "cc"}]
        self.assertEqual(
            "ccc", replace_code(rules, "cat\ncat\ncat"), "Not same in many lines remove"
        )

    def test_many_lines(self):
        rules = [{"replace": "at", "by": "cc"}]
        self.assertEqual(
            "ccc\nccc\nccc",
            replace_code(rules, "cat\ncat\ncat"),
            "Not same in many lines",
        )

    def test_many_rules(self):
        rules = [{"replace": "at", "by": "bb"}, {"replace": "og", "by": "aa"}]
        self.assertEqual(
            "daa\ncbb",
            replace_code(rules, "dog\ncat"),
            "Not same in many rules",
        )

    def test_delete_lines(self):
        by_code_replace = [
            {"replace": r"(\n[^\n]*DELETEBEGIN.*? DELETEEND[^\n]*)", "by": ""}
        ]
        lines = """one
two
# DELETEBEGIN
three
four
# DELETEEND
five"""

        result = """one
two
five"""
        self.assertEqual(
            result,
            replace_code(by_code_replace, lines),
            "Not same in delete lines",
        )

    def test_replace_by_code(self):
        by_code_replace = [
            {
                "replace": r"((\n|)[^\n]*BYCODEBEGIN.*?BYCODEEND[^\n]*)",
                "by": "\nREPLACEBYCODE",
            }
        ]
        lines = """one
            two
            # BYCODEBEGIN
            three
            four
            # BYCODEEND
            five"""

        result = """one
            two
REPLACEBYCODE
            five"""
        self.assertEqual(
            result,
            replace_code(by_code_replace, lines),
            "Not same in replace by code",
        )
