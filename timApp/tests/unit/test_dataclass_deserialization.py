from dataclasses import dataclass
from typing import Union
from unittest import TestCase

from marshmallow import ValidationError

from marshmallow_dataclass import class_schema


class DataclassDeserializationTest(TestCase):

    def test_deserialization(self):
        @dataclass
        class A:
            x: int

        a_s = class_schema(A)
        with self.assertRaises(ValidationError):
            a_s().load({'x': 0.1})
        with self.assertRaises(ValidationError):
            a_s().load({'x': '0.1'})
        v = a_s().load({'x': '1'})
        self.assertEqual(1, v.x)
        v = a_s().load({'x': 2})
        self.assertEqual(2, v.x)
        self.assertIsInstance(v.x, int)

        @dataclass
        class B:
            x: Union[int, float]

        b_s = class_schema(B)
        v = b_s().load({'x': 2})
        self.assertEqual(2, v.x)
        self.assertIsInstance(v.x, int)
        self.assertEqual(2.1, b_s().load({'x': 2.1}).x)
        self.assertEqual(0.1, b_s().load({'x': 0.1}).x)
        self.assertEqual(2, b_s().load({'x': '2'}).x)
