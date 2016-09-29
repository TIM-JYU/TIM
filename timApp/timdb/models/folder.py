from typing import Optional

from documentmodel.document import Document
from tim_app import db
from timdb.blocktypes import blocktypes
from timdb.dbutils import insert_block, copy_default_rights
from timdb.models.docentry import DocEntry
from timdb.timdbexception import TimDbException
from utils import split_location, join_location

ROOT_FOLDER_ID = -1


class Folder(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'folder'
    id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    name = db.Column(db.Text, nullable=False)
    location = db.Column(db.Text, nullable=False)

    @staticmethod
    def get_root() -> 'Folder':
        return Folder(id=ROOT_FOLDER_ID, name='', location='')

    @staticmethod
    def get_by_id(fid) -> Optional['Folder']:
        return Folder.query.get(fid) if fid != ROOT_FOLDER_ID else Folder.get_root()

    @staticmethod
    def find_by_location(location, name) -> Optional['Folder']:
        return Folder.query.filter_by(name=name, location=location).first()

    @staticmethod
    def find_by_full_path(path) -> Optional['Folder']:
        parent_loc, name = split_location(path)
        return Folder.find_by_location(parent_loc, name)

    def is_root(self) -> bool:
        return self.id == -1

    def get_parent(self) -> Optional['Folder']:
        if self.is_root():
            return None
        parent_loc, name = split_location(self.location)
        return Folder.find_by_location(parent_loc, name) if name else Folder.get_root()

    def get_full_path(self) -> str:
        return join_location(self.location, self.name)

    def get_document(self, relative_path: str, create_if_not_exist=False, creator_group_id: int = None) -> Optional[
        DocEntry]:
        doc = DocEntry.query.filter_by(name=join_location(self.get_full_path(), relative_path)).first()
        if doc is not None:
            doc.document = Document(doc_id=doc.id)
            return doc
        if create_if_not_exist:
            assert creator_group_id is not None
            rel_folder, _ = split_location(relative_path)
            Folder.create(join_location(self.get_full_path(), rel_folder), owner_group_id=creator_group_id,
                          commit=False)
            return DocEntry.create(join_location(self.get_full_path(), relative_path), owner_group_id=creator_group_id)
        else:
            return None

    @staticmethod
    def create(path: str, owner_group_id: int, commit=True, apply_default_rights=False) -> 'Folder':
        """Creates a new folder with the specified name. If the folder already exists, it is returned.

        :param apply_default_rights: Whether to apply default rights from parents.
        :param commit: Whether to commit the changes.
        :param path: The name of the folder to be created.
        :param owner_group_id: The id of the owner group.
        :returns: The created or existing folder.
        """

        if '\0' in path:
            raise TimDbException('Folder name cannot contain null characters.')

        path = path.strip('/')

        # Root folder is special case
        if not path:
            return Folder.get_root()

        rel_path, rel_name = split_location(path)
        folder = Folder.query.filter_by(name=rel_name, location=rel_path).first()
        if folder is not None:
            return folder

        block_id = insert_block(path, owner_group_id, blocktypes.FOLDER, commit=False)

        f = Folder(id=block_id, name=rel_name, location=rel_path)
        db.session.add(f)

        # Make sure that the parent folder exists
        Folder.create(rel_path, owner_group_id, commit=False)

        if apply_default_rights:
            copy_default_rights(f.id, blocktypes.FOLDER)

        if commit:
            db.session.commit()

        return f
