import datetime
import json

from isodate import duration_isoformat
from sqlalchemy.ext.declarative import DeclarativeMeta


class TimJsonEncoder(json.JSONEncoder):

    def default(self, o):
        if isinstance(o, datetime.datetime):
            if o.tzinfo is None:
                o = o.replace(tzinfo=datetime.timezone.utc)
            return o.isoformat()

        if isinstance(o, datetime.timedelta):
            return duration_isoformat(o)

        # from http://stackoverflow.com/a/31569287 with some changes
        if isinstance(o.__class__, DeclarativeMeta):
            if hasattr(o, 'to_json'):
                return o.to_json()
            data = {}
            fields = o.__json__() if hasattr(o, '__json__') else dir(o)
            for field in [f for f in fields if not f.startswith('_') and f not in ['metadata', 'query', 'query_class']]:
                value = o.__getattribute__(field)
                try:
                    json.dumps(value)
                    data[field] = value
                except TypeError:
                    data[field] = None
            return data

        from documentmodel.docparagraph import DocParagraph
        if isinstance(o, DocParagraph):  # currently not used anywhere
            return {'md': o.get_markdown(),
                    'html': o.get_html(),
                    't': o.get_hash(),
                    'id': o.get_id(),
                    'attrs': o.get_attrs()}
