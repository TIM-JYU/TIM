from sqlalchemy import UniqueConstraint
from sqlalchemy.orm import mapped_column

from timApp.document.docinfo import DocInfo
from timApp.timdb.sqa import db


class Translation(db.Model, DocInfo):
    """A translated document.

    Translation objects may be created in two scenarios:

    - An existing non-translated document is assigned a language.
    - A new translated document is created (via manage view).

    """

    __tablename__ = "translation"
    
    
    doc_id = mapped_column(db.Integer, db.ForeignKey("block.id"), primary_key=True)
    src_docid = mapped_column(db.Integer, db.ForeignKey("block.id"), nullable=False)
    lang_id = mapped_column(db.Text, nullable=False)
    __table_args__ = (UniqueConstraint("src_docid", "lang_id", name="translation_uc"),)

    _block = db.relationship(
        "Block", back_populates="translation", foreign_keys=[doc_id]
    )

    docentry = db.relationship(
        "DocEntry",
        back_populates="trs",
        primaryjoin="foreign(Translation.src_docid) == DocEntry.id",
    )

    @property
    def path(self):
        return (
            self.path_without_lang + "/" + self.lang_id
            if self.lang_id
            else self.path_without_lang
        )

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
    def translations(self) -> list["Translation"]:
        return self.docentry.trs

    def to_json(self, **kwargs):
        return {
            **super().to_json(**kwargs),
            "src_docid": self.src_docid,
            "lang_id": self.lang_id,
        }


def add_tr_entry(doc_id: int, item: DocInfo, tr: Translation) -> Translation:
    new_tr = Translation(doc_id=doc_id, src_docid=item.id, lang_id=tr.lang_id)
    db.session.add(new_tr)
    new_tr.title = tr.title
    # Set docentry so that it can be used without extra queries in other methods
    new_tr.docentry = item
    return new_tr
