import random
import unittest
from ratelimiter import RateLimited


class TestException(Exception):
    pass


class RateLimitTest(unittest.TestCase):

    def testNoParams(self):
        def raises_exception():
            raise TestException()

        limited = RateLimited(raises_exception, 10, 2)

        self.assertRaises(TestException, limited)
        self.assertRaises(TestException, limited)
        self.assertIsNone(limited())
        self.assertIsNone(limited())

        for i in range(0,3):
            limited.update(2)
            self.assertIsNone(limited())
            limited.update(2)
            self.assertIsNone(limited())

            limited.update(6)
            self.assertRaises(TestException, limited)
            self.assertRaises(TestException, limited)
            self.assertIsNone(limited())
            self.assertIsNone(limited())

    def testReturnValue(self):
        def always_true():
            return True

        limited = RateLimited(always_true, 10, 2, default_value=False)

        self.assertTrue(limited())
        self.assertTrue(limited())
        self.assertFalse(limited())
        self.assertFalse(limited())

        for i in range(0,3):
            limited.update(2)
            self.assertFalse(limited())
            limited.update(2)
            self.assertFalse(limited())

            limited.update(6)
            self.assertTrue(limited())
            self.assertTrue(limited())
            self.assertFalse(limited())
            self.assertFalse(limited())


    def testFullParams(self):
        def sum(a, b):
            return a + b

        limited = RateLimited(sum, 10, 2)

        self.assertEqual(limited(3, 2), 5)
        self.assertEqual(limited(6, 8), 14)
        self.assertIsNone(limited())
        self.assertIsNone(limited())

        for i in range(0, 3):
            limited.update(2)
            self.assertIsNone(limited())
            limited.update(2)
            self.assertIsNone(limited())

            a = random.randrange(0, 100)
            b = random.randrange(0, 100)

            limited.update(6)
            self.assertEqual(limited(a, b), a + b)
            self.assertEqual(limited(b, a), b + a)
            self.assertIsNone(limited())
            self.assertIsNone(limited())
