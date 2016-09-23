from documentmodel.document import Document
from tim_app import db
from timdb.blocktypes import blocktypes
from timdb.dbutils import insert_block
from timdb.timdbexception import TimDbException
from utils import split_location


class DocEntry(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'docentry'
    name = db.Column(db.Text, primary_key=True)
    id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    public = db.Column(db.Boolean, nullable=False, default=True)

    document = None  # type: Document

    @staticmethod
    def create(name: str, owner_group_id: int) -> 'DocEntry':
        """Creates a new document with the specified name.

        :param name: The name of the document to be created (can be None).
        :param owner_group_id: The id of the owner group (can be None).
        :returns: The newly created document object.
        """

        if '\0' in name:
            raise TimDbException('Document name cannot contain null characters.')

        document_id = insert_block(name, owner_group_id, blocktypes.DOCUMENT, commit=False)
        document = Document(document_id, modifier_group_id=owner_group_id)
        document.create()

        docentry = DocEntry(id=document_id, name=name, public=True)
        db.session.add(docentry)
        db.session.commit()
        docentry.document = document
        return docentry

    def get_parent(self):
        folder, name = split_location(self.name)
        from timdb.models.folder import Folder
        return Folder.find_by_full_path(folder) if folder else Folder(id=-1)
