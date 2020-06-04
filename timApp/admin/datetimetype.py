from datetime import datetime
from typing import Any

import click
import dateutil.parser


class DateTimeType(click.ParamType):
    """A better datetime type to use with Click."""
    name = "datetime"

    def convert(self, value: str, param: str, ctx: Any) -> datetime:
        try:
            return dateutil.parser.parse(value)
        except (ValueError, OverflowError) as e:
            return self.fail(str(e), param, ctx)
