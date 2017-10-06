"""Server tests for randomization."""
import ast

from timApp.tests.server.timroutetest import TimRouteTest, get_content


class RandomTest(TimRouteTest):
    def test_rnd_s(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {rnd="s10"}
%%rnd%%
""")
        nums = ast.literal_eval(get_content(self.get(d.url, as_tree=True))[0])
        self.assertIsInstance(nums, list)
        self.assertEqual(sorted(nums), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
        nums2 = ast.literal_eval(get_content(self.get(d.url, as_tree=True))[0])
        self.assertEqual(nums, nums2)  # should be cached
