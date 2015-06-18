import json
from document.docparagraph import DocParagraph


class DocParagraphEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, DocParagraph):
            return {'md': o.getMarkdown(), 'html': o.getHtml(), 't': o.getHash(), 'id': o.getId()}
