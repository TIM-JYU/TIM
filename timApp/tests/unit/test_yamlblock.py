import unittest

# noinspection PyPackageRequirements
import yaml

# noinspection PyPackageRequirements
from yaml import YAMLError

from timApp.document.yamlblock import (
    YamlBlock,
    MergeStyle,
    yaml_loader,
    BlockEndMissingError,
    DuplicateKeyMergeHintError,
    InvalidIndentError,
)


class YamlBlockTest(unittest.TestCase):
    md1 = """
macros:
 first: a
 second: b
css: |!!
.red {
    color: red;
}
!!
    """
    md2 = """
macros:
 second: c
 third: d
css: |!!
.blue {
    color: blue;
}
!!
    """
    combined_replace = """
macros:
 first: a
 second: c
 third: d
css: |!!
.blue {
    color: blue;
}
!!
        """
    combined_append = """
macros:
 first: a
 second: c
 third: d
css: |!! a
.red {
    color: red;
}
.blue {
    color: blue;
}
!!
    """

    multiple_multiline = """
a: |!!
test1
!!
b: |??
test2
??
    """

    def test_empty(self):
        self.assertEqual({}, YamlBlock.from_markdown(""))

    def test_parse(self):
        yb = YamlBlock.from_markdown(self.md1)
        self.assertEqual(
            {
                "macros": {"first": "a", "second": "b"},
                "css": ".red {\n    color: red;\n}\n",
            },
            yb,
        )

    def test_merge_replace(self):
        yb = YamlBlock.from_markdown(self.md1)
        yb2 = YamlBlock.from_markdown(self.md2)
        ybc = YamlBlock.from_markdown(self.combined_append)
        self.assertEqual(ybc.values, yb.merge_with(yb2).values)

        yb2 = YamlBlock.from_markdown(self.md2.replace("|!!", "|!! r"))
        self.assertEqual(yb2.merge_hints, {"css": MergeStyle.Replace})
        self.assertEqual(
            YamlBlock.from_markdown(self.combined_replace).values,
            yb.merge_with(yb2).values,
        )

    def test_merge_replace_if_not_exist(self):
        yb = YamlBlock.from_markdown(self.md1)
        yb2 = YamlBlock.from_markdown(self.md2.replace("|!!", "|!! r?"))
        self.assertEqual({"css": MergeStyle.ReplaceIfNotExist}, yb2.merge_hints)
        self.assertEqual(
            {
                "css": ".red {\n    color: red;\n}\n",
                "macros": {"first": "a", "second": "c", "third": "d"},
            },
            yb.merge_with(yb2).values,
        )

    def test_merge_append(self):
        yb = YamlBlock.from_markdown(self.md1)
        self.assertEqual({}, yb.merge_hints)
        yb2 = YamlBlock.from_markdown(self.md2.replace("|!!", "|!! a"))
        self.assertEqual({"css": MergeStyle.Append}, yb2.merge_hints)
        ybc = YamlBlock.from_markdown(self.combined_append)
        self.assertEqual(yb.merge_with(yb2).values, ybc.values)

    def test_invalid(self):
        invalid = [
            "css: !!!",
            "css: !!",
            """
"a: |!!":|
 asd
 asd
""",
            """
'a: |!!':|
 asd
 asd
""",
        ]
        for md in invalid:
            with self.assertRaises(YAMLError) as cm:
                YamlBlock.from_markdown(md)
            self.assertNotIsInstance(cm.exception, BlockEndMissingError)

    def test_standard(self):
        standard = [
            "css: !",
            "css:",
            "css: ",
            " css:",
            " css: ",
            "css: |+\n a\n b",
            "css: |+\n a\n b\n",
            "css: |-\n a\n b",
            "css: |-\n a\n b\n",
        ]
        for md in standard:
            yb = YamlBlock.from_markdown(md)
            self.assertEqual(
                yaml.load(md, yaml_loader),
                yb.values,
                msg=f"\nFailed YAML:\n-----\n{md}\n-----",
            )

    def test_missing_end(self):
        with self.assertRaises(BlockEndMissingError) as e:
            YamlBlock.from_markdown("css: |!!\ntest\n")
        self.assertEqual("!!", e.exception.end_str)
        with self.assertRaises(BlockEndMissingError) as e:
            YamlBlock.from_markdown("css: |xx")
        self.assertEqual("xx", e.exception.end_str)

    def test_empty_multiline_key(self):
        yb = YamlBlock.from_markdown("css: |!!\n!!")
        self.assertEqual({"css": ""}, yb.values)

    def test_one_char_terminator(self):
        yb = YamlBlock.from_markdown("css: |!\nhello\n!\n")
        self.assertEqual({"css": "hello\n"}, yb)

    def test_multiple_multiline(self):
        yb = YamlBlock.from_markdown(self.multiple_multiline)
        self.assertEqual({"a": "test1\n", "b": "test2\n"}, yb.values)

    def test_same_key_diff_level(self):
        yb = YamlBlock.from_markdown(
            """
a: test
macros:
 a: test2
        """
        )
        self.assertEqual(yb, {"a": "test", "macros": {"a": "test2"}})

    def test_multiline_append_deeper(self):
        yb = YamlBlock.from_markdown(
            """
macros:
 a: |!!
test
ing
!!
 b: test2"""
        )
        self.assertEqual({"macros": {"a": "test\ning\n", "b": "test2"}}, yb.values)
        yb2 = YamlBlock.from_markdown(
            """
macros:
 a: |!! a
continued
!!
 b: xxx"""
        )
        ybm = yb.merge_with(yb2)
        self.assertEqual(
            {"macros": {"a": "test\ning\ncontinued\n", "b": "xxx"}}, ybm.values
        )

    def test_duplicate_key_append(self):
        invalid_mds = [
            """
a: |!! a
yyy
!!
macros:
 a: |!! a
test
!!
        """,
            """
a: yyy
macros:
 a: |!! a
test
!!
        """,
            """
a: |!! a
yyy
!!
macros:
 a: test
        """,
        ]
        for md in invalid_mds:
            with self.assertRaises(DuplicateKeyMergeHintError) as e:
                YamlBlock.from_markdown(md)
            self.assertEqual(
                'Using merge hints in a key ("a") having same name in different levels is not currently supported',
                str(e.exception),
            )

    def test_multiline_invalid_indentation(self):
        with self.assertRaises(InvalidIndentError) as e:
            YamlBlock.from_markdown(
                """
a: |!!
 t
b
!!
        """
            )
        self.assertEqual(
            'The line "b" must be indented at least as much as the first line.',
            str(e.exception),
        )
        yb = YamlBlock.from_markdown(
            """
a: |!!
 t
  b
!!
                """
        )
        self.assertEqual({"a": "t\n b\n"}, yb)

    def test_empty_line_auto_indent(self):
        yb = YamlBlock.from_markdown(
            """
a: |!!
 t

 b
!!
                """
        )
        self.assertEqual({"a": "t\n\nb\n"}, yb)

    def test_standard_indent_marker(self):
        yb = YamlBlock.from_markdown(
            """
a: |1
 x
"""
        )
        self.assertEqual({"a": "x\n"}, yb)

        yb = YamlBlock.from_markdown(
            """
a: |9
           x
"""
        )
        self.assertEqual({"a": "  x\n"}, yb)

        yb = YamlBlock.from_markdown(
            """
a: |1
"""
        )
        self.assertEqual({"a": ""}, yb)

        yb = YamlBlock.from_markdown(
            """
a: |9
"""
        )
        self.assertEqual({"a": ""}, yb)

    def test_anchor_depth(self):
        def parse():
            YamlBlock.from_markdown(
                """
a: &a ["lol","lol","lol","lol","lol","lol","lol","lol","lol"]
b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]
c: &c [*b,*b,*b,*b,*b,*b,*b,*b,*b]
d: &d [*c,*c,*c,*c,*c,*c,*c,*c,*c]
e: &e [*d,*d,*d,*d,*d,*d,*d,*d,*d]
f: &f [*e,*e,*e,*e,*e,*e,*e,*e,*e]
g: &g [*f,*f,*f,*f,*f,*f,*f,*f,*f]
h: &h [*g,*g,*g,*g,*g,*g,*g,*g,*g]
i: &i [*h,*h,*h,*h,*h,*h,*h,*h,*h]
"""
            )

        self.assertRaises(YAMLError, parse)

    def test_css_default_append(self):
        yb = YamlBlock.from_markdown("css: 'html {display: none;}'")
        yb2 = YamlBlock.from_markdown("css: 'body {display: none;}'")
        result = yb.merge_with(yb2)
        self.assertEqual({"css": "html {display: none;}body {display: none;}"}, result)

    def test_merge_hint_comment(self):
        yb = YamlBlock.from_markdown("a: 1 #r\nb: [1,2] #a\nc: x #r?\nd: y #r!\ne: z")
        self.assertEqual(
            {
                "a": MergeStyle.Replace,
                "b": MergeStyle.Append,
                "c": MergeStyle.ReplaceIfNotExist,
                "d": MergeStyle.ReplaceOnlyIfExists,
            },
            yb.merge_hints,
        )
        self.assertEqual({"a": 1, "b": [1, 2], "c": "x", "d": "y", "e": "z"}, yb.values)

    def test_merge_hint_comment_not_hint(self):
        yb = YamlBlock.from_markdown("a: 1 #abc\nb: x#a\nc: 'q #a'\nd: 2 # r\n#e: 3 #a")
        self.assertEqual({}, yb.merge_hints)

    def test_merge_hint_comment_duplicate(self):
        with self.assertRaises(DuplicateKeyMergeHintError):
            YamlBlock.from_markdown("a: 1 #r\na: 2")

    def test_merge_list_append_comment(self):
        yb = YamlBlock.from_markdown("a: [1, 2]")
        yb2 = YamlBlock.from_markdown("a: [3] #a")
        self.assertEqual({"a": [1, 2, 3]}, yb.merge_with(yb2).values)
        yb3 = YamlBlock.from_markdown("a: [3]")
        self.assertEqual({"a": [3]}, yb.merge_with(yb3).values)

    def test_merge_dict_replace(self):
        yb = YamlBlock.from_markdown("a:\n  x: 1\n  y: 2")
        yb2 = YamlBlock.from_markdown("a: #r\n  y: 3\n  z: 4")
        self.assertEqual({"a": MergeStyle.Replace}, yb2.merge_hints)
        self.assertEqual(
            {"a": {"y": 3, "z": 4}},
            yb.merge_with(yb2).values,
            "Merge with replace hint failed",
        )
        yb3 = YamlBlock.from_markdown("a:\n  y: 3\n  z: 4")
        self.assertEqual({"a": {"x": 1, "y": 3, "z": 4}}, yb.merge_with(yb3).values)

    def test_merge_dict_replace_if_not_exist(self):
        yb = YamlBlock.from_markdown("a:\n  x: 1")
        yb2 = YamlBlock.from_markdown("a: #r?\n  y: 3\nb: #r?\n  z: 4")
        self.assertEqual({"a": {"x": 1}, "b": {"z": 4}}, yb.merge_with(yb2).values)

    def test_merge_replace_only_if_exists(self):
        yb = YamlBlock.from_markdown("a: 1\nb:\n  x: 1")
        yb2 = YamlBlock.from_markdown("a: 2 #r!\nb: #r!\n  y: 2\nc: 3 #r!\nd: 4")
        self.assertEqual({"a": 2, "b": {"y": 2}, "d": 4}, yb.merge_with(yb2).values)

    def test_merge_different_types_and_equal(self):
        yb = YamlBlock.from_markdown("a: 1\nb: [1]\nc: x")
        yb2 = YamlBlock.from_markdown("a: text\nb: [1] #a\nc: x")
        self.assertEqual({"a": "text", "b": [1], "c": "x"}, yb.merge_with(yb2).values)

    def test_merge_hints_combined(self):
        yb = YamlBlock.from_markdown("a: 1 #r")
        yb2 = YamlBlock.from_markdown("b: [1] #a")
        self.assertEqual(
            {"a": MergeStyle.Replace, "b": MergeStyle.Append},
            yb.merge_with(yb2, do_merge_hints=True).merge_hints,
        )

    def test_merge_set_append(self):
        yb = YamlBlock.from_markdown("a: !!set {x, y}")
        yb2 = YamlBlock.from_markdown("a: !!set {z} #a")
        self.assertEqual({"a": {"x", "y", "z"}}, yb.merge_with(yb2).values)

    def test_merge_set_replace(self):
        yb = YamlBlock.from_markdown("a: !!set {x, y}")
        yb2 = YamlBlock.from_markdown("a: !!set {z} #r")
        self.assertEqual({"a": {"z"}}, yb.merge_with(yb2).values)

    def test_merge_multiline_replace_only_if_exists(self):
        yb = YamlBlock.from_markdown(self.md1)
        yb2 = YamlBlock.from_markdown(self.md2.replace("|!!", "|!! r!"))
        self.assertEqual(yb2.merge_hints, {"css": MergeStyle.ReplaceOnlyIfExists})
        self.assertEqual(
            YamlBlock.from_markdown(self.combined_replace).values,
            yb.merge_with(yb2).values,
        )
        yb3 = YamlBlock.from_markdown("macros:\n a: b")
        self.assertEqual(
            {"macros": {"a": "b", "second": "c", "third": "d"}},
            yb3.merge_with(yb2).values,
        )

    def test_merge_multiline_append_comment_list(self):
        yb = YamlBlock.from_markdown("a: [1]\ntext: |!!\nx\nx2\n!!\n")
        yb2 = YamlBlock.from_markdown("a: [2] #a\ntext: |!! #a\ny\ny2\n!!\n")
        self.assertEqual(
            {"a": [1, 2], "text": "x\nx2\ny\ny2\n"}, yb.merge_with(yb2).values
        )

    def test_merge_multiline_hint_with_hash(self):
        for h, style in (
            ("a", MergeStyle.Append),
            ("r", MergeStyle.Replace),
            ("r?", MergeStyle.ReplaceIfNotExist),
            ("r!", MergeStyle.ReplaceOnlyIfExists),
        ):
            for prefix in ("", "#"):
                yb = YamlBlock.from_markdown(
                    self.md2.replace("|!!", f"|!! {prefix}{h}")
                )
                self.assertEqual({"css": style}, yb.merge_hints)
