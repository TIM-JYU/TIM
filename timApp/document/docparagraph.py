from contracts import new_contract


class DocParagraph:
    def __init__(self, md, html, par_id, t, attrs=None):
        if not attrs:
            attrs = {}
        self.md = md
        self.html = html
        self.id = par_id
        self.t = t
        self.attrs = attrs

new_contract('DocParagraph', DocParagraph)
