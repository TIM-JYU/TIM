from typing import List

from sqlalchemy import UniqueConstraint

from timdb.docinfo import DocInfo
from timdb.tim_models import db


class Translation(db.Model, DocInfo):
    __bind_key__ = 'tim_main'
    __tablename__ = 'translation'
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    src_docid = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    lang_id = db.Column(db.Text, nullable=False)
    __table_args__ = (UniqueConstraint('src_docid', 'lang_id', name='translation_uc'),
                      )

    _block = db.relationship('Block', foreign_keys=[doc_id])

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
    def public(self):
        return self.docentry.public

    @property
    def translations(self) -> List['Translation']:
        return Translation.query.filter_by(src_docid=self.src_docid).all()

    @staticmethod
    def find_by_docentry(d: 'DocEntry'):
        trs = Translation.query.filter_by(src_docid=d.id).all()
        for tr in trs:
            tr.docentry = d
        return trs

    def to_json(self):
        return {**super(Translation, self).to_json(),
                'src_docid': self.src_docid,
                'lang_id': self.lang_id}
