import datetime
import json
from dataclasses import is_dataclass, fields
from enum import Enum
from typing import Any

from flask.json.provider import JSONProvider
from isodate import duration_isoformat
from isodate.duration import Duration
from jinja2 import Undefined
from marshmallow import missing

try:
    from sqlalchemy.ext.declarative import DeclarativeMeta
    from sqlalchemy.orm import DeclarativeBase

    sqlalchemy_imported = True
except ImportError:
    sqlalchemy_imported = False


class TimJsonProvider(JSONProvider):
    def dumps(self, obj: Any, **kwargs: Any) -> str:
        return json.dumps(obj, cls=TimJsonEncoder, **kwargs)

    def loads(self, s: str | bytes, **kwargs: Any) -> Any:
        return json.loads(s, **kwargs)


SQA_DBMODEL_ATTRS = {
    "metadata",
    "query",
    "query_class",
    "registry",
    "type_annotation_map",
}


class TimJsonEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, datetime.datetime):
            if o.tzinfo is None:
                o = o.replace(tzinfo=datetime.timezone.utc)
            return o.isoformat()

        if isinstance(o, datetime.date):
            return o.isoformat()

        if isinstance(o, (datetime.timedelta, Duration)):
            return duration_isoformat(o)

        if isinstance(o, Undefined):
            return None

        tojson = getattr(o, "to_json", None)
        if tojson:
            return tojson()
        # from http://stackoverflow.com/a/31569287 with some changes
        if sqlalchemy_imported and (
            isinstance(o, DeclarativeMeta) or isinstance(o, DeclarativeBase)
        ):
            data = {}
            if hasattr(o, "__json__"):
                flds = o.__json__()
            else:
                flds = dir(o)
                flds = [
                    f
                    for f in flds
                    if not f.startswith("_") and f not in SQA_DBMODEL_ATTRS
                ]
            for field in flds:
                value = o.__getattribute__(field)
                try:
                    json.dumps(value, cls=TimJsonEncoder)
                    data[field] = value
                except TypeError:
                    data[field] = None
            return data
        if isinstance(o, Enum):
            return o.value
        if is_dataclass(o):
            return {
                f.name: val
                for f in fields(o)
                if (val := getattr(o, f.name)) is not missing
            }
        return None
