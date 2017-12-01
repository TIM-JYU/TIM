import datetime
import json

from isodate import duration_isoformat
from jinja2 import Undefined
from sqlalchemy.ext.declarative import DeclarativeMeta


class TimJsonEncoder(json.JSONEncoder):

    def default(self, o):
        # open("/service/timApp/Output.txt", "a").write("self: "+str(self) + " o:" + str(o) + "\n")
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
                fields = o.__json__()
            else:
                fields = dir(o)
                fields = [f for f in fields if not f.startswith('_') and f not in ['metadata', 'query', 'query_class']]
            for field in fields:
                value = o.__getattribute__(field)
                try:
                    json.dumps(value, cls=TimJsonEncoder)
                    data[field] = value
                except TypeError:
                    data[field] = None
            return data

        from timApp.documentmodel.docparagraph import DocParagraph
        if isinstance(o, DocParagraph):  # currently not used anywhere
            return {'md': o.get_markdown(),
                    'html': o.get_html(),
                    't': o.get_hash(),
                    'id': o.get_id(),
                    'attrs': o.get_attrs()}
