# const numFilterEx: RegExp = /([<=>!]=?) *(-?[\w.,]*) *(!?) */g;
import re
from operator import ge, gt, ne, eq, le, lt
from typing import List, Union, Tuple

num_filter_expression = '([<=>!]=?) *(-?[\\w.,]*) *(!?) *'


# const filterComparatorOperators = {
#     "<": ((a: NumStr, b: NumStr) => a < b),
#     "<=": ((a: NumStr, b: NumStr) => a <= b),
#     "=": ((a: NumStr, b: NumStr) => a === b),
#     "==": ((a: NumStr, b: NumStr) => a === b),
#     "!==": ((a: NumStr, b: NumStr) => a !== b),
#     "!=": ((a: NumStr, b: NumStr) => a !== b),
#     "!": ((a: NumStr, b: NumStr) => a !== b),
#     ">": ((a: NumStr, b: NumStr) => a > b),
#     ">=": ((a: NumStr, b: NumStr) => a >= b),
# };

def float_or_str_tuple(a: Union[str, float], b: Union[str, float]) -> Union[Tuple[str, str], Tuple[float, float]]:
    """
    Attempts to convert two parameters to float
    :param a: first parameter to convert
    :param b: second parameter to convert
    :return: Pair of floats if both succeed, else pair of strings
    """
    try:
        float_a = float(a)
        float_b = float(b)
        return float_a, float_b
    except ValueError:
        str_a = str(a)
        str_b = str(b)
        return str_a, str_b


comparatorOperators = {
    "<": lt,
    "<=": le,
    "=": eq,
    "==": eq,
    "!==": ne,
    "!=": ne,
    "!": ne,
    ">": gt,
    ">=": ge,
}


def is_num_filter(s: str) -> bool:
    return s[0] in "!<=>"


class RegexOrComparator:
    """
    Class for creating custom filter from string
    If string starts with !<=>, we treat it as custom comparator where the symbol defines the comparison function
    Else the string will be used as generic (case-insensitive) regex.
    See compatorfilter.ts for original custom comparator rules
    """

    def __init__(self, fltr: str):
        if not is_num_filter(fltr):
            self.reg = rf'{re.escape(fltr.lower())}'
            self.is_comparator = False
        else:
            self.comparator = ComparatorFilter(fltr)
            self.is_comparator = True

    def is_match(self, value: Union[str, float, None]) -> bool:
        if not self.is_comparator:
            if value is None:
                field_value = ''
            elif not isinstance(value, str):
                field_value = str(value)
            else:
                field_value = value
            if re.search(self.reg, field_value.lower()):
                return True
        else:
            if self.comparator.is_match(value):
                return True
        return False


class ComparatorFilter:
    def __init__(self, fltr: str):
        self.values: List[Union[str, float]] = []
        self.funcs = []
        self.negate = False
        for result in re.findall(num_filter_expression, fltr):
            op = result[0]
            self.funcs.append(comparatorOperators.get(op, eq))
            vs = result[1]
            if result[2]:
                self.negate = False
            try:
                v = float(vs)
            except ValueError:
                v = vs.lower()
            self.values.append(v)

    def is_match(self, s: Union[str, float, None]) -> bool:
        if s is None:
            s = ''
        if isinstance(s, str):
            s = s.lower()
        res = True
        for i, my_val in enumerate(self.values):
            targ_val_cast, my_val_cast = float_or_str_tuple(s, my_val)
            if not self.funcs[i](targ_val_cast, my_val_cast):
                res = False
                break
        return not res if self.negate else res
