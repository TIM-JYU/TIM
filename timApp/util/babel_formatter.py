import string
from typing import Any

from babel.numbers import format_decimal

from timApp.util.locale import get_locale


class BabelFormatter(string.Formatter):
    def format_field(self, value: Any, format_spec: str):
        """
        Additional formatter that can handle Babel number formatting.
        If the format spec ends with 'b', it will use Babel to format the number according to the current locale.

        :param value:
        :param format_spec:
        :return:
        """
        if format_spec.endswith("b"):
            format_spec = format_spec[:-1]
            return format_decimal(
                value, format=format_spec or None, locale=get_locale()
            )
        return super().format_field(value, format_spec)


babel_fmt = BabelFormatter()
