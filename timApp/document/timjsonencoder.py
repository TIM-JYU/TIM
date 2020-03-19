import datetime
import json
from dataclasses import is_dataclass, fields
from enum import Enum

from isodate import duration_isoformat
from jinja2 import Undefined
from sqlalchemy.ext.declarative import DeclarativeMeta


class TimJsonEncoder(json.JSONEncoder):

    def default(self, o):
        if isinstance(o, datetime.datetime):
            if o.tzinfo is None:
                o = o.replace(tzinfo=datetime.timezone.utc)
            return o.isoformat()

        if isinstance(o, datetime.timedelta):
            return duration_isoformat(o)

        if isinstance(o, Undefined):
            return None

        tojson = getattr(o, 'to_json', None)
        if tojson:
            return tojson()
        # from http://stackoverflow.com/a/31569287 with some changes
        if isinstance(o.__class__, DeclarativeMeta):
            data = {}
            if hasattr(o, '__json__'):
                flds = o.__json__()
            else:
                flds = dir(o)
                flds = [f for f in flds if not f.startswith('_') and f not in ['metadata', 'query', 'query_class']]
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
            return {f.name: getattr(o, f.name) for f in fields(o)}
        from timApp.document.docparagraph import DocParagraph
        if isinstance(o, DocParagraph):  # currently not used anywhere
            return {'md': o.get_markdown(),
                    'html': o.get_html(),
                    't': o.get_hash(),
                    'id': o.get_id(),
                    'attrs': o.get_attrs()}
