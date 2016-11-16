from typing import Optional, Union

from documentmodel.document import Document
from timdb.gamification_models.gamificationdocument import GamificationDocument
from timdb.docinfo import DocInfo
from timdb.tim_models import db
from timdb.models.translation import Translation
from timdb.blocktypes import blocktypes
from timdb.dbutils import insert_block
from timdb.timdbexception import TimDbException
from utils import split_location


class DocEntry(db.Model, DocInfo):
    __bind_key__ = 'tim_main'
    __tablename__ = 'docentry'
    name = db.Column(db.Text, primary_key=True)
    id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    public = db.Column(db.Boolean, nullable=False, default=True)

    block = db.relationship('Block', backref=db.backref('docentries', lazy='dynamic'))

    @property
    def path(self):
        return self.name

    @property
    def path_without_lang(self):
        return self.name

    @staticmethod
    def find_by_id(doc_id: int, try_translation=False) -> Union['DocEntry', 'Translation']:
        d = DocEntry.query.filter_by(id=doc_id).first()
        if d:
            return d
        if try_translation:
            d = Translation.query.get(doc_id)
        return d

    @staticmethod
    def find_by_path(path: str, fallback_to_id=False, try_translation=False) -> Union['DocEntry', 'Translation']:
        d = DocEntry.query.get(path)
        if d:
            return d
        # try translation
        if try_translation:
            base_doc_path, lang = split_location(path)
            entry = DocEntry.find_by_path(base_doc_path)
            if entry is not None:
                tr = Translation.query.filter_by(src_docid=entry.id, lang_id=lang).first()
                if tr is not None:
                    tr.docentry = entry
                    return tr
        if fallback_to_id:
            try:
                return DocEntry.find_by_id(int(path))
            except ValueError:
                return None
        return d

    @staticmethod
    def get_dummy(title):
        return DocEntry(id=-1, name=title)

    @staticmethod
    def create(name: Optional[str], owner_group_id: int, is_gamified: bool = False) -> 'DocEntry':
        """Creates a new document with the specified name.

        :param name: The name of the document to be created (can be None). If None, no DocEntry is actually added
         to the database; only Block and Document objects are created.
        :param owner_group_id: The id of the owner group.
        :param is_gamified: --MADE BY TIMG-- Boolean value whether the document is gamified
        :returns: The newly created document object.
        """

        if name is not None and '\0' in name:
            raise TimDbException('Document name cannot contain null characters.')

        document_id = insert_block(name, owner_group_id, blocktypes.DOCUMENT, commit=False)
        document = Document(document_id, modifier_group_id=owner_group_id)
        document.create()

        docentry = DocEntry(id=document_id, name=name, public=True)
        if name is not None:
            db.session.add(docentry)

        if is_gamified:
            GamificationDocument.create(document_id)

        db.session.commit()
        return docentry
