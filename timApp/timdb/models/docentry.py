from typing import Optional

from documentmodel.document import Document
from timdb.gamification_models.gamificationdocument import GamificationDocument
from timdb.tim_models import db
from timdb.blocktypes import blocktypes
from timdb.dbutils import insert_block
from timdb.timdbexception import TimDbException
from utils import split_location, date_to_relative


class DocEntry(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'docentry'
    name = db.Column(db.Text, primary_key=True)
    id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    public = db.Column(db.Boolean, nullable=False, default=True)

    block = db.relationship('Block', backref=db.backref('docentries', lazy='dynamic'))

    def __getattr__(self, item):
        if item == 'document':
            self.document = Document(self.id)
            return self.document
        raise AttributeError

    @staticmethod
    def find_by_id(doc_id: int) -> Optional['DocEntry']:
        d = DocEntry.query.filter_by(id=doc_id).first()
        return d

    @staticmethod
    def find_by_path(path: str) -> Optional['DocEntry']:
        d = DocEntry.query.get(path)
        return d

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

    def get_parent(self):
        folder, name = split_location(self.name)
        from timdb.models.folder import Folder
        return Folder.find_by_full_path(folder) if folder else Folder(id=-1)

    def get_short_name(self) -> str:
        parts = self.name.rsplit('/', 1)
        return parts[len(parts) - 1]

    def get_path(self):
        return self.name

    def get_title(self) -> str:
        return self.block.description

    def get_last_modified(self):
        return self.document.get_last_modified()

    def to_json(self):
        from routes.accesshelper import get_rights
        return {'name': self.get_short_name(),
                'fullname': self.get_path(),
                'id': self.id,
                'modified': date_to_relative(self.get_last_modified()),
                'isFolder': False,
                'owner': self.block.owner,
                'rights': get_rights(self.id),
                'unpublished': self.block.is_unpublished()
                }
