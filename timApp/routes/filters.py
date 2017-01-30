from datetime import datetime, timedelta

import humanize
from jinja2.utils import soft_unicode


def map_format(value, pattern):
    """Apply python string formatting on an object:

    .. sourcecode:: jinja
        {{ "%s - %s"|format("Hello?", "Foo!") }}
            -> Hello? - Foo!

    """
    return soft_unicode(pattern) % value


def timdate(value: datetime):
    return value.isoformat()


def humanize_timedelta(value: timedelta):
    return humanize.naturaldelta(value)


def humanize_datetime(value: datetime):
    return humanize.naturaltime(value.replace(tzinfo=None))
