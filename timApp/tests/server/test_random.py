"""Server tests for randomization."""
import ast

from timApp.tests.server.timroutetest import TimRouteTest, get_content
from timApp.util.rndutils import SeedClass, get_rnds


class RandomTest(TimRouteTest):
    def test_rnd_s(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {rnd="s10"}
%%rnd%%
"""
        )
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
            settings={"rndmacros": {"first": "s3*[1,8]"}},
        )
        nums = self.get_number_list(d, 1)
        self.assertEqual(3, len(nums))
        self.assertEqual(3, len(set(nums)))
        self.assertTrue(all(1 <= x <= 8 for x in nums))
        self.assertEqual(nums, self.get_number_list(d, 1))

    def test_rnd_i(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {rnd="i[1,7]"}
%%rnd%%
"""
        )
        nums = self.get_number_list(d)
        # A plain paragraph has no attempt counter, so an m-list stays on the
        # first value of the first cycle.
        self.assertEqual(1, len(nums))
        self.assertIn(nums[0], range(1, 8))
        self.assertEqual(nums, self.get_number_list(d))  # should be cached

    @staticmethod
    def walk(spec, attempts, seed=12345, attrs=None):
        """Values an i-list gives on successive attempts.

        SeedClass.extraseed is what askNew increases by one for every new
        version of a task, so counting it up here stands for pressing askNew.
        """
        return [
            get_rnds({"rnd": spec, **(attrs or {})}, "rnd", SeedClass(seed, i))[0]
            for i in range(attempts)
        ]

    def test_distinct_cycle_uses_every_value_once(self):
        nums = [v[0] for v in self.walk("i[1,7]", 7)]
        self.assertEqual([1, 2, 3, 4, 5, 6, 7], sorted(nums))
        self.assertNotEqual([1, 2, 3, 4, 5, 6, 7], nums)  # and in a shuffled order

    def test_distinct_reshuffles_on_every_cycle(self):
        nums = [v[0] for v in self.walk("i[1,7]", 14)]
        first, second = nums[:7], nums[7:]
        self.assertEqual(sorted(first), sorted(second))
        self.assertNotEqual(first, second)

    def test_distinct_no_repeat_until_pool_is_used_up(self):
        # Includes the wrap: a cycle never starts with the value the cycle
        # before it ended with.
        for size in range(2, 10):
            for seed in range(20):
                nums = [v[0] for v in self.walk(f"i[1,{size}]", size * 4, seed)]
                for start in range(0, len(nums), size):
                    cycle = nums[start : start + size]
                    self.assertEqual(size, len(set(cycle)), msg=f"{size=} {seed=}")
                for i in range(1, len(nums)):
                    self.assertNotEqual(
                        nums[i - 1], nums[i], msg=f"{size=} {seed=} {i=}"
                    )

    def test_distinct_short_pools(self):
        # One value cannot avoid repeating; two have no order left to choose.
        self.assertEqual([[5]] * 4, self.walk("i[5,5]", 4))
        nums = [v[0] for v in self.walk("i[1,2]", 6)]
        self.assertTrue(all(a != b for a, b in zip(nums, nums[1:])))

    def test_distinct_many_values_per_attempt(self):
        attempts = self.walk("i3:[1,9]", 3)
        self.assertTrue(all(len(a) == 3 for a in attempts))
        # Three attempts of three values use up the pool of nine exactly once.
        self.assertEqual(list(range(1, 10)), sorted(v for a in attempts for v in a))

    def test_distinct_step_and_bare_forms(self):
        self.assertEqual([1, 3, 5, 7], sorted(v[0] for v in self.walk("i[1,7,2]", 4)))
        # Both ends of the range belong to it, as with s.
        bare = [v[0] for v in self.walk("i10", 11)]
        self.assertEqual(list(range(0, 11)), sorted(bare))
        self.assertEqual(bare, [v[0] for v in self.walk("i1:10", 11)])
        self.assertEqual(bare, [v[0] for v in self.walk("i[10]", 11)])

    def test_distinct_without_attempt_counter(self):
        # A seed that is not a SeedClass carries no counter, so the walk stays
        # on the first value, and stays there for good.
        nums, _, _ = get_rnds({"rnd": "i[1,7]"}, "rnd", 12345)
        self.assertEqual(self.walk("i[1,7]", 1)[0], nums)
        self.assertEqual(nums, get_rnds({"rnd": "i[1,7]"}, "rnd", 12345)[0])

    def test_distinct_same_attempt_gives_same_value(self):
        self.assertEqual(self.walk("i[1,7]", 20), self.walk("i[1,7]", 20))
        self.assertNotEqual(self.walk("i[1,7]", 20), self.walk("i[1,7]", 20, 999))

    def test_distinct_bad_range(self):
        # ValueError is what insert_rnds turns into a message in the document.
        for spec in ["i", "i[1,7,0]", "i[1,900]"]:
            with self.assertRaises(ValueError, msg=spec):
                get_rnds({"rnd": spec}, "rnd", SeedClass(1, 0))

    def test_distinct_with_new_task_seed(self):
        # A paragraph is only a new task, and so only gets its attempts
        # counted, when it has seed="answernr". That is therefore the seed an
        # i-list is used with, and it must not put the pool in a new order on
        # every attempt.
        nums = [v[0] for v in self.walk("i[1,7]", 14, attrs={"seed": "answernr"})]
        self.assertEqual([1, 2, 3, 4, 5, 6, 7], sorted(nums[:7]))
        self.assertEqual([1, 2, 3, 4, 5, 6, 7], sorted(nums[7:]))
        for i in range(1, len(nums)):
            self.assertNotEqual(nums[i - 1], nums[i], msg=f"{i=}")
