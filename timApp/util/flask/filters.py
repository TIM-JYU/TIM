from datetime import datetime, timedelta

import humanize
from markupsafe import soft_unicode


def map_format(value, pattern):
    """Applies Python string formatting on an object:

    .. sourcecode:: jinja

       {{ "%s - %s"|format("Hello?", "Foo!") }}

    gives::

        Hello? - Foo!

    """
    return soft_unicode(pattern) % value


def timdate(value: datetime):
    return value.isoformat()


def humanize_timedelta(value: timedelta):
    return humanize.naturaldelta(value)


def humanize_datetime(value: datetime):
    return humanize.naturaltime(value.replace(tzinfo=None))
