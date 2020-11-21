from unittest import TestCase
from timApp.markdown.markdownconverter import srange

class TestSRange(TestCase):

    def test_srange_normal(self):
         r = srange("d{0} ", 1, 3)
         e = "d1 d2 d3 "
         self.assertEqual(e, r, "Not same in normal case")

    def test_srange_step(self):
         r = srange("d{0} ", 1, 5, 2)
         e = "d1 d3 d5 "
         self.assertEqual(e, r, "Not same step")

    def test_srange_plus(self):
        r = srange("d{0}-{1} ", 1, 3, 1, 10)
        e = "d1-11 d2-12 d3-13 "
        self.assertEqual(e, r, "Not same add")

    def test_srange_mul(self):
        r = srange("d{0}-{1} ", 1, 3, 1, 0, 3)
        e = "d1-3 d2-6 d3-9 "
        self.assertEqual(e, r, "Not same mul")

    def test_srange_mul_add(self):
        r = srange("d{0}-{1}-{2} ", 1, 3, 1, 0, 3, 2, 5)
        e = "d1-3-7 d2-6-12 d3-9-17 "
        self.assertEqual(e, r, "Not same mul")
