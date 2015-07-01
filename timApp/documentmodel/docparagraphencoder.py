import json
from documentmodel.docparagraph import DocParagraph


class DocParagraphEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, DocParagraph):
            return {'md': o.get_markdown(),
                    'html': o.getHtml(),
                    't': o.get_hash(),
                    'id': o.get_id(),
                    'attrs': o.getAttrs()}
