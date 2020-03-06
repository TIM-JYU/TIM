from unittest import TestCase

from timApp.plugin.plugin import find_inline_plugins_from_str


class TestFindInlinePlugins(TestCase):
    def test_find_inline_plugins1(self):
        #               1         2         3         4         5         6         7         8
        #     0123456789012345678901234567890123456 78901234567890123456789012345678901234567890
        s1 = "Pinnat {#yhtpisteet2 autosave: true,\ninputstem: Pisteet yhteensä, verticalkeys: true, header: '*kana*', stem: '[Puh]{.red}'#}"

        r1 = find_inline_plugins_from_str(s1)
        for p_task_id, p_yaml, p_range, md in r1:
            self.assertEqual("yhtpisteet2", p_task_id.s, "Not same task_id")
            self.assertEqual(
                " autosave: true,\ninputstem: Pisteet yhteensä, verticalkeys: true, header: '*kana*', stem: '[Puh]{.red}'",
                p_yaml, "Not same yaml")
            self.assertEqual("(7, 125)", str(p_range), "Not same range")

    def test_find_inline_plugins2(self):
        #               1         2
        #     012345678901234567890
        s1 = "{#jono#}"

        r1 = find_inline_plugins_from_str(s1)
        for p_task_id, p_yaml, p_range, md in r1:
            self.assertEqual("jono", p_task_id.s, "Not same task_id")
            self.assertEqual("", p_yaml, "Not same yaml")
            self.assertEqual("(0, 8)", str(p_range), "Not same range")

    def test_find_inline_plugins3(self):
        #               1         2
        #     012345678901234567890
        s1 = "{# aaa#}"

        r1 = find_inline_plugins_from_str(s1)
        for p_task_id, p_yaml, p_range, md in r1:
            self.assertEqual("", p_task_id.s, "Not same task_id")
            self.assertEqual(" aaa", p_yaml, "Not same yaml")
            self.assertEqual("(0, 8)", str(p_range), "Not same range")

    def test_find_inline_plugins4(self):
        #               1         2         3         4         5         6         7         8
        #     012345678901234567890123456789012345678901234567890123456789012345678901234567890
        s1 = "{#test:dropdown words: [option 1, option 2, option 3]#}"

        r1 = find_inline_plugins_from_str(s1)
        for p_task_id, p_yaml, p_range, md in r1:
            self.assertEqual("test:dropdown", p_task_id.s, "Not same task_id")
            self.assertEqual(" words: [option 1, option 2, option 3]", p_yaml, "Not same yaml")
            self.assertEqual("(0, 55)", str(p_range), "Not same range")

    def test_find_inline_plugins5(self):
        #               1         2         3         4         5         6         7         8
        #     012345678901234567890123456789012345678901234567890123456789012345678901234567890
        s1 = "Eka {#test1 a: 5#} toka {#t2 b: 3#} kolmas"

        p = ["test1", "t2"]
        y = [" a: 5", " b: 3"]
        r = ["(4, 18)", "(24, 35)"]

        r1 = find_inline_plugins_from_str(s1)
        i = 0
        for p_task_id, p_yaml, p_range, md in r1:
            self.assertEqual(p[i], p_task_id.s, "Not same task_id")
            self.assertEqual(y[i], p_yaml, "Not same yaml")
            self.assertEqual(r[i], str(p_range), "Not same range")
            i += 1

    def test_find_inline_plugins6(self):
        #               1         2
        #     012345678901234567890
        s1 = "{#234.jono#}"

        r1 = find_inline_plugins_from_str(s1)
        for p_task_id, p_yaml, p_range, md in r1:
            self.assertEqual("234.jono", p_task_id.s, "Not same task_id")
            self.assertEqual("", p_yaml, "Not same yaml")
            self.assertEqual("(0, 12)", str(p_range), "Not same range")

    def test_find_inline_plugins7(self):
        #               1         2
        #     012345678901234567890
        s1 = "{##}"

        r1 = find_inline_plugins_from_str(s1)
        for p_task_id, p_yaml, p_range, md in r1:
            self.assertEqual("", p_task_id.s, "Not same task_id")
            self.assertEqual("", p_yaml, "Not same yaml")
            self.assertEqual("(0, 4)", str(p_range), "Not same range")
