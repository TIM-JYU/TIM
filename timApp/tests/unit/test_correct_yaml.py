from unittest import TestCase
from timApp.document.yamlblock import correct_yaml,compare_same

class TestCorrect_yaml(TestCase):
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
second:|!!
one
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
 one
   two
third: |
 one
   two
"""
        a1, h = correct_yaml(s1)
        self.assertEqual(e1, a1, "Not same in normal case")

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
one
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
 one
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
second:@!!
one:
 two:@!
three: 
  four:1
!  
!!
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
