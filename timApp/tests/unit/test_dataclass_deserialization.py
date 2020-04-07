from dataclasses import dataclass
from typing import Union, Any
from unittest import TestCase

from marshmallow import ValidationError

from marshmallow_dataclass import class_schema
from utils import Missing


class DataclassDeserializationTest(TestCase):

    def test_float_deserialization(self):
        @dataclass
        class A:
            x: int

        a_s = class_schema(A)()
        with self.assertRaises(ValidationError):
            a_s.load({'x': 0.1})
        with self.assertRaises(ValidationError):
            a_s.load({'x': '0.1'})
        v = a_s.load({'x': '1'})
        self.assertEqual(1, v.x)
        v = a_s.load({'x': 2})
        self.assertEqual(2, v.x)
        self.assertIsInstance(v.x, int)

        @dataclass
        class B:
            x: Union[int, float]

        b_s = class_schema(B)()
        v = b_s.load({'x': 2})
        self.assertEqual(2, v.x)
        self.assertIsInstance(v.x, int)
        self.assertEqual(2.1, b_s.load({'x': 2.1}).x)
        self.assertEqual(0.1, b_s.load({'x': 0.1}).x)
        self.assertEqual(2, b_s.load({'x': '2'}).x)

    def test_any_deserialization(self):
        @dataclass
        class A:
            x: Any

        a_s = class_schema(A)()
        self.assertEqual(1, a_s.load({'x': 1}).x)
        self.assertEqual('a', a_s.load({'x': 'a'}).x)
        self.assertEqual(1.2, a_s.load({'x': 1.2}).x)
        self.assertEqual({}, a_s.load({'x': {}}).x)
        self.assertEqual(None, a_s.load({'x': None}).x)

        @dataclass
        class B:
            x: Union[Any, Missing]

        a_s = class_schema(B)()
        self.assertEqual(1, a_s.load({'x': 1}).x)
        self.assertEqual('a', a_s.load({'x': 'a'}).x)
        self.assertEqual(1.2, a_s.load({'x': 1.2}).x)
        self.assertEqual({}, a_s.load({'x': {}}).x)
        self.assertEqual(None, a_s.load({'x': None}).x)
