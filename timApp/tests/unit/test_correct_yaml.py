from unittest import TestCase
from timApp.document.yamlblock import correct_yaml, compare_same, YamlBlock, MergeStyle


class TestCorrectYaml(TestCase):
    def test_comparesame(self):
        self.assertEqual(True, compare_same("cat", "cat", 0), "t0")
        self.assertEqual(True, compare_same("  cat", "cat", 2), "t1")
        self.assertEqual(True, compare_same("  cat", "cat", 4), "t2")
        self.assertEqual(False, compare_same("  cat", "cat", 1), "t3")
        self.assertEqual(True, compare_same("cat", "cat", 1), "t4")

    ##############################################################
    def test_correct_yaml1(self):
        s1 = """
first:1
second:|!! a
one
  two
!!
third:|+1!!
 one
   two
!!
"""
        e1 = """
first: 1
second: |
 one
   two
third: |+1
 one
   two
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in normal case")
        self.assertEqual(h["second"].value, "a", "Hint wrong")

    ##############################################################

    def test_correct_yaml2(self):
        s1 = """
zero:0
one:
  two:
    third:|##
t0
 t1
t2     
##
"""
        e1 = """
zero: 0
one:
  two:
    third: |
     t0
      t1
     t2
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in multiple object case")

        ##############################################################

    def test_correct_yaml3(self):
        s1 = """
first:1
second:|!!
one:
  two
!!
   third:|!!
     one
     two
   !!
"""
        e1 = """
first: 1
second: |
 one:
   two
   third: |
     one
     two
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in normal case")

    ##############################################################
    def test_correct_yaml4(self):
        s1 = """
first:1
second:@!!1
one: @!2
two:@!3
three:@!4
four:1
!4
!3
!2  
!!1
third:|!!
    one
    two
    !!
"""
        e1 = """
first: 1
second:
 one:
  two:
   three:
    four: 1
third: |
    one
    two
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in object case")

    ##############################################################
    def test_correct_yaml5(self):
        s1 = """
first:1
second:@!!1
one: @!2
two:@!3
three:|!4
string
!4
!3
!2  
!!1
third:|!!
    one
    two
    !!
"""
        e1 = """
first: 1
second:
 one:
  two:
   three: |
    string
third: |
    one
    two
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in object case 5")

    ##############################################################
    def test_correct_yaml6(self):
        s1 = """
a1: |!!
cat
  o1: @!
a:1
!
!!
"""
        e1 = """
a1: |
 cat
   o1: @!
 a:1
 !
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in object inside string")

    ##############################################################
    def test_correct_yaml7(self):
        s1 = """
a1: |a1jono
cat
  o1: @!
a:1
!
a1jono
"""
        e1 = """
a1: |
 cat
   o1: @!
 a:1
 !
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in object inside string, string named")

    ##############################################################
    def test_correct_yaml8(self):
        s1 = """
teksti: |!!
 Tekstia
!!
olio:
    ali:@!
      name: Kalle
!
"""
        e1 = """
teksti: |
 Tekstia
olio:
    ali:
      name: Kalle
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in string before object, string named")

    ##############################################################
    def test_correct_yaml9(self):
        s1 = """
three:|!4
string
!4
third: |
  one:a
four:
  long: |
    a:1
    b:2
 end     
"""
        e1 = """
three: |
 string
third: |
  one:a
four:
  long: |
    a:1
    b:2
 end
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in normal multiline")

    ##############################################################
    def test_correct_yaml10(self):
        s1 = """
avain:"joopa
http://koti"
avain:'joopa
http://koti'
avain:joopa
  https://koti
"""
        e1 = """
avain: "joopa
http://koti"
avain: 'joopa
http://koti'
avain: joopa
  https://koti
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in prevent http:")

    def test_folded_unindented(self):
        yb = YamlBlock.from_markdown("a: >!! a\nx\ny\n!!\nb: 1")
        self.assertEqual({"a": "x y\n", "b": 1}, yb.values)
        self.assertEqual({"a": MergeStyle.Append}, yb.merge_hints)

    def test_multiline_hint_with_comment(self):
        md = "css: |!!{}\n.blue {{\n    color: blue;\n}}\n!!\n"
        yb = YamlBlock.from_markdown(md.format(" #aa"))
        self.assertEqual({}, yb.merge_hints)
        yb = YamlBlock.from_markdown(md.format(" #a a"))
        self.assertEqual({"css": MergeStyle.Append}, yb.merge_hints)

    def test_multiline_zero_indent(self):
        yb = YamlBlock.from_markdown("a: |0!!\n  x\ny\n!!\nb: 1")
        self.assertEqual({"a": "  x\ny\n", "b": 1}, yb.values)

    def test_multiline_zero_indent_fullprogram(self):
        yb = YamlBlock.from_markdown(
            "type: shell\nfullprogram: |0!!\n   # BYCODEBEGIN\nprint('Hello world!')\n# BYCODEEND\n!!\nmacros:"
        )
        self.assertEqual(
            "   # BYCODEBEGIN\nprint('Hello world!')\n# BYCODEEND\n",
            yb.values["fullprogram"],
        )

    def test_multiline_zero_indent_twice(self):
        yb = YamlBlock.from_markdown("a: |0!!\n  x\ny\n!!\nb: |0!!\n z\nw\n!!\nc: 1")
        self.assertEqual({"a": "  x\ny\n", "b": " z\nw\n", "c": 1}, yb.values)

    def test_multiline_zero_indent_strip(self):
        yb = YamlBlock.from_markdown("a: |-0!!\nx\ny\n!!")
        self.assertEqual({"a": "x\ny"}, yb.values)

    def test_multiline_indicator_twice(self):
        yb = YamlBlock.from_markdown("a: |1!!\n  x\n y\n!!\nb: |+1aa\n z\naa\n")
        self.assertEqual(" x\ny\n", yb.values["a"])
        self.assertEqual("z\n", yb.values["b"])
