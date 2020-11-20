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
        nums = self.get_number_list(d)
        self.assertIsInstance(nums, list)
        self.assertEqual(sorted(nums), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
        nums2 = self.get_number_list(d)
        self.assertEqual(nums, nums2)  # should be cached

    def get_number_list(self, d, index=0):
        return ast.literal_eval(get_content(self.get(d.url, as_tree=True))[index])

    def test_doc_rnds(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {nocache=true}
%%first%%
        """,
            settings={'rndmacros': {'first': 's3*[1,8]'}})
        nums = self.get_number_list(d, 1)
        self.assertEqual(3, len(nums))
        self.assertEqual(3, len(set(nums)))
        self.assertTrue(all(1 <= x <= 8 for x in nums))
        self.assertEqual(nums, self.get_number_list(d, 1))
