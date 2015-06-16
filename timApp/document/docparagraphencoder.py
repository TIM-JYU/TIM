import json
from document.docparagraph import DocParagraph


class DocParagraphEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, DocParagraph):
            return {'md': o.md, 'html': o.html, 't': o.t, 'id': o.id}
