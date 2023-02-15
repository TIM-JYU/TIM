from unittest import TestCase
from timApp.modules.cs.cs_utils import check_parsons


class TestCheckParsons(TestCase):
    def test_empty_usercode(self):
        p, c = check_parsons("cat\ndog", "", 10, False)
        self.assertEqual(0, p, "Failed empty usercode")
        self.assertEqual([-1, -1], c, "Failed empty usercode c")

    def test_full_match(self):
        p, c = check_parsons("cat\ndog", "cat\ndog", 10, False)
        self.assertEqual(1, p, "Failed full match")
        self.assertEqual([1, 1], c, "Failed full match c")

    def test_2_lines(self):
        p, c = check_parsons("cat\ndog\nfox", "cat\ndog\nmouse", 2, False)
        self.assertEqual(1, p, "Failed 2 lines")
        self.assertEqual([1, 1], c, "Failed 2 lines c")

    def test_2_lines_no_order(self):
        p, c = check_parsons("cat\ndog\nfox", "dog\ncat\nmouse", 2, True)
        self.assertEqual(1, p, "Failed 2 lines no order")
        self.assertEqual([1, 1], c, "Failed 2 lines no order c")

    def test_too_many(self):
        p, c = check_parsons("cat\ndog", "cat\ndog\nmouse", 10, False)
        self.assertEqual(0, p, "Failed too many")
        self.assertEqual([1, 1, -1], c, "Failed too many c")

    def test_too_few(self):
        p, c = check_parsons("cat\ndog\nfox", "cat\ndog", 10, False)
        self.assertEqual(0, p, "Failed too few")
        self.assertEqual([1, 1, -1], c, "Failed too few c")

    def test_too_many_no_order(self):
        p, c = check_parsons("cat\ndog", "cat\ndog\nmouse", 10, True)
        self.assertEqual(0, p, "Failed too many no order")
        self.assertEqual([1, 1, -1], c, "Failed too many no order c")

    def test_too_few_no_order(self):
        p, c = check_parsons("cat\ndog\nfox", "cat\ndog", 10, True)
        self.assertEqual(0, p, "Failed too few no order")
        self.assertEqual([1, 1, -1], c, "Failed too few no order c")

    def test_remove_end(self):
        rules = [{"replace": "===.*", "by": ""}]
        p, c = check_parsons(
            "cat\ndog\n===\nfox", "cat\ndog\n===\nmouse\nfox", 10, False, rules
        )
        self.assertEqual(1, p, "Failed remove end")
        self.assertEqual([1, 1], c, "Failed remove end c")
