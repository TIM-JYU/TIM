from contracts import new_contract


class DocParagraph:
    def __init__(self, md, html, par_id, t):
        self.md = md
        self.html = html
        self.id = par_id
        self.t = t

new_contract('DocParagraph', DocParagraph)
