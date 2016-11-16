from timdb.docinfo import DocInfo
from timdb.tim_models import db


class Translation(db.Model, DocInfo):
    __bind_key__ = 'tim_main'
    __tablename__ = 'translation'
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    src_docid = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    lang_id = db.Column(db.Text, nullable=False)
    doc_title = db.Column(db.Text)

    block = db.relationship('Block', foreign_keys=[doc_id])

    @property
    def docentry(self):
        if not hasattr(self, '_docentry'):
            from timdb.models.docentry import DocEntry
            self._docentry = DocEntry.find_by_id(self.src_docid)
        return self._docentry

    @docentry.setter
    def docentry(self, value):
        self._docentry = value

    @property
    def path(self):
        return self.path_without_lang + '/' + self.lang_id

    @property
    def id(self):
        return self.doc_id

    @property
    def path_without_lang(self):
        return self.docentry.path

    @property
    def title(self):
        return self.doc_title
