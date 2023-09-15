import sys
from dataclasses import field
from typing import Any, Mapping, overload

import marshmallow
from isodate import Duration, duration_isoformat, parse_duration
from marshmallow import ValidationError
from marshmallow.fields import Boolean
from marshmallow.utils import _Missing

Missing = _Missing
Missing.__hash__ = lambda self: id(self)  # type: ignore

#

missing_field = field(default_factory=lambda: marshmallow.missing)  # type: ignore

_BoolField = Boolean()


def parse_bool(value: Any, default: bool = False) -> bool:
    try:
        return _BoolField.deserialize(value)
    except ValidationError:
        return default


def safe_parse_item_list(item_list: str) -> list[str]:
    """
    Read an item list of format

        - item1
        - item2
        - item3

    and return a list of strings.
    Any indentation is flattened and empty lines/empty items are ignored.
    The starting dash is optional.

    Example:
    >>> safe_parse_item_list('''- item1\n- item2\n- item3''')
    ['item1', 'item2', 'item3']
    >>> safe_parse_item_list('''
    ... - item1
    ... - item2
    ...    - item3
    ...- item4
    ...    item5''')
    ['item1', 'item2', 'item3', 'item4', 'item5']

    :param item_list: Item list to parse
    :return: List of items
    """

    result = []

    for line in item_list.splitlines():
        line = line.strip()
        if line.startswith("-"):
            line = line[1:].lstrip()
        if line:
            result.append(line)

    return result


@overload
def round_float_error(v: None) -> None:
    ...


@overload
def round_float_error(v: float) -> float:
    ...


def round_float_error(v: float | None) -> float | None:
    """
    Round floats to remove any round-off errors.

    Example:
    >>> round_float_error(0.1 + 0.2)
    >>> 0.3
    >>> round_float_error(1.8 + 0.1)
    >>> 1.9

    :param v: Value to round
    :return: Rounded value
    """
    if v is None:
        return None
    return round(v, sys.float_info.dig)


class DurationField(marshmallow.fields.Field):
    def _serialize(
        self, value: Duration, attr: str | None, obj: Any, **kwargs: dict[str, Any]
    ) -> str:
        return duration_isoformat(value)

    def _deserialize(
        self,
        value: Any,
        attr: str | None,
        data: Mapping[str, Any] | None,
        **kwargs: dict[str, Any]
    ) -> Duration:
        try:
            return parse_duration(value)
        except:
            raise self.make_error("invalid")


class DurationSchema(marshmallow.Schema):
    TYPE_MAPPING = {Duration: DurationField}
