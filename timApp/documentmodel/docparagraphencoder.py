import datetime
import json
from documentmodel.docparagraph import DocParagraph
from plugin import date_format


class DocParagraphEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, DocParagraph):
            return {'md': o.get_markdown(),
                    'html': o.get_html(),
                    't': o.get_hash(),
                    'id': o.get_id(),
                    'attrs': o.get_attrs()}
        if isinstance(o, datetime.datetime):
            return o.strftime(date_format)
