import unittest

import yaml
from yaml import YAMLError

from timApp.document.yamlblock import YamlBlock, MergeStyle, yaml_loader, BlockEndMissingError, \
    DuplicateKeyMergeHintError, InvalidIndentError


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
        self.assertEqual(YamlBlock.from_markdown(''), {})

    def test_parse(self):
        yb = YamlBlock.from_markdown(self.md1)
        self.assertEqual(yb,
                         {'macros': {'first': 'a', 'second': 'b'}, 'css': '.red {\n    color: red;\n}\n'})

    def test_merge_replace(self):
        yb = YamlBlock.from_markdown(self.md1)
        yb2 = YamlBlock.from_markdown(self.md2)
        ybc = YamlBlock.from_markdown(self.combined_append)
        self.assertEqual(yb.merge_with(yb2).values, ybc.values)

        yb2 = YamlBlock.from_markdown(self.md2.replace('|!!', '|!! r'))
        self.assertEqual(yb2.merge_hints, {'css': MergeStyle.Replace})
        self.assertEqual(yb.merge_with(yb2).values, YamlBlock.from_markdown(self.combined_replace).values)

    def test_merge_replace_if_not_exist(self):
        yb = YamlBlock.from_markdown(self.md1)
        yb2 = YamlBlock.from_markdown(self.md2.replace('|!!', '|!! r?'))
        self.assertEqual(yb2.merge_hints, {'css': MergeStyle.ReplaceIfNotExist})
        self.assertEqual(yb.merge_with(yb2).values, {'css': '.red {\n    color: red;\n}\n',
                                                     'macros': {'first': 'a', 'second': 'c', 'third': 'd'}})

    def test_merge_append(self):
        yb = YamlBlock.from_markdown(self.md1)
        self.assertEqual(yb.merge_hints, {})
        yb2 = YamlBlock.from_markdown(self.md2.replace('|!!', '|!! a'))
        self.assertEqual(yb2.merge_hints, {'css': MergeStyle.Append})
        ybc = YamlBlock.from_markdown(self.combined_append)
        self.assertEqual(yb.merge_with(yb2).values, ybc.values)

    def test_invalid(self):
        invalid = [
            'css: !!!',
            'css: !!',
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
            'css: !',
            'css:',
            'css: ',
            ' css:',
            ' css: ',
            'css: |+\n a\n b',
            'css: |+\n a\n b\n',
            'css: |-\n a\n b',
            'css: |-\n a\n b\n',
        ]
        for md in standard:
            yb = YamlBlock.from_markdown(md)
            self.assertEqual(yb.values, yaml.load(md, yaml_loader), msg=f'\nFailed YAML:\n-----\n{md}\n-----')

    def test_missing_end(self):
        with self.assertRaises(BlockEndMissingError) as e:
            YamlBlock.from_markdown('css: |!!\ntest\n')
        self.assertEqual(e.exception.end_str, '!!')
        with self.assertRaises(BlockEndMissingError) as e:
            YamlBlock.from_markdown('css: |xx')
        self.assertEqual(e.exception.end_str, 'xx')

    def test_empty_multiline_key(self):
        yb = YamlBlock.from_markdown('css: |!!\n!!')
        self.assertEqual(yb.values, {'css': ''})

    def test_one_char_terminator(self):
        yb = YamlBlock.from_markdown('css: |!\nhello\n!')
        self.assertEqual(yb, {'css': 'hello'})

    def test_multiple_multiline(self):
        yb = YamlBlock.from_markdown(self.multiple_multiline)
        self.assertEqual(yb.values, {'a': 'test1\n', 'b': 'test2\n'})

    def test_same_key_diff_level(self):
        yb = YamlBlock.from_markdown("""
a: test
macros:
 a: test2
        """)
        self.assertEqual(yb, {'a': 'test', 'macros': {'a': 'test2'}})

    def test_multiline_append_deeper(self):
        yb = YamlBlock.from_markdown("""
macros:
 a: |!!
test
ing
!!
 b: test2""")
        self.assertEqual(yb.values, {'macros': {'a': 'test\ning\n', 'b': 'test2'}})
        yb2 = YamlBlock.from_markdown("""
macros:
 a: |!! a
continued
!!
 b: xxx""")
        ybm = yb.merge_with(yb2)
        self.assertEqual(ybm.values, {'macros': {'a': 'test\ning\ncontinued\n', 'b': 'xxx'}})

    def test_duplicate_key_append(self):
        invalid_mds = ["""
a: |!! a
yyy
!!
macros:
 a: |!! a
test
!!
        """, """
a: yyy
macros:
 a: |!! a
test
!!
        """, """
a: |!! a
yyy
!!
macros:
 a: test
        """]
        for md in invalid_mds:
            with self.assertRaises(DuplicateKeyMergeHintError) as e:
                YamlBlock.from_markdown(md)
            self.assertEqual(
                'Using merge hints in a key ("a") having same name in different levels is not currently supported',
                str(e.exception))

    def test_multiline_invalid_indentation(self):
        with self.assertRaises(InvalidIndentError) as e:
            YamlBlock.from_markdown("""
a: |!!
 t
b
!!
        """)
        self.assertEqual(
            'The line "b" must be indented at least as much as the first line.',
            str(e.exception))
        yb = YamlBlock.from_markdown("""
a: |!!
 t
  b
!!
                """)
        self.assertEqual({'a': 't\n b\n'}, yb)

    def test_empty_line_auto_indent(self):
        yb = YamlBlock.from_markdown("""
a: |!!
 t

 b
!!
                """)
        self.assertEqual({'a': 't\n\nb\n'}, yb)

    def test_standard_indent_marker(self):
        yb = YamlBlock.from_markdown("""
a: |1
 x
""")
        self.assertEqual({'a': 'x\n'}, yb)

        yb = YamlBlock.from_markdown("""
a: |9
           x
""")
        self.assertEqual({'a': '  x\n'}, yb)

        yb = YamlBlock.from_markdown("""
a: |1
""")
        self.assertEqual({'a': ''}, yb)

        yb = YamlBlock.from_markdown("""
a: |9
""")
        self.assertEqual({'a': ''}, yb)

    def test_anchor_depth(self):
        def parse():
            YamlBlock.from_markdown("""
a: &a ["lol","lol","lol","lol","lol","lol","lol","lol","lol"]
b: &b [*a,*a,*a,*a,*a,*a,*a,*a,*a]
c: &c [*b,*b,*b,*b,*b,*b,*b,*b,*b]
d: &d [*c,*c,*c,*c,*c,*c,*c,*c,*c]
e: &e [*d,*d,*d,*d,*d,*d,*d,*d,*d]
f: &f [*e,*e,*e,*e,*e,*e,*e,*e,*e]
g: &g [*f,*f,*f,*f,*f,*f,*f,*f,*f]
h: &h [*g,*g,*g,*g,*g,*g,*g,*g,*g]
i: &i [*h,*h,*h,*h,*h,*h,*h,*h,*h]
""")
        self.assertRaises(YAMLError, parse)

    def test_css_default_append(self):
        yb = YamlBlock.from_markdown("css: 'html {display: none;}'")
        yb2 = YamlBlock.from_markdown("css: 'body {display: none;}'")
        result = yb.merge_with(yb2)
        self.assertEqual({'css': 'html {display: none;}body {display: none;}'}, result)
