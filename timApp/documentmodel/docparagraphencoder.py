import datetime
import json

from sqlalchemy.ext.declarative import DeclarativeMeta

from documentmodel.docparagraph import DocParagraph


class DocParagraphEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, DocParagraph):
            return {'md': o.get_markdown(),
                    'html': o.get_html(),
                    't': o.get_hash(),
                    'id': o.get_id(),
                    'attrs': o.get_attrs()}
        if isinstance(o, datetime.datetime):
            if o.tzinfo is None:
                o = o.replace(tzinfo=datetime.timezone.utc)
            return o.isoformat()

        # from http://stackoverflow.com/a/31569287
        if isinstance(o.__class__, DeclarativeMeta):
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
