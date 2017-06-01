import os
import re

from typing import List, Iterable
from typing import Optional

from timdb.item import Item
from timdb.models.block import Block
from timdb.tim_models import db, BlockAccess
from timdb.blocktypes import blocktypes
from timdb.dbutils import insert_block, copy_default_rights
from timdb.models.docentry import DocEntry
from timdb.timdbexception import TimDbException
from utils import split_location, join_location

ROOT_FOLDER_ID = -1


class Folder(db.Model, Item):
    __bind_key__ = 'tim_main'
    __tablename__ = 'folder'
    id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    name = db.Column(db.Text, nullable=False)
    location = db.Column(db.Text, nullable=False)

    _block = db.relationship('Block', backref=db.backref('folder', lazy='dynamic'))

    @staticmethod
    def get_root() -> 'Folder':
        # noinspection PyArgumentList
        return Folder(id=ROOT_FOLDER_ID, name='', location='')

    @staticmethod
    def get_by_id(fid) -> Optional['Folder']:
        return Folder.query.get(fid) if fid != ROOT_FOLDER_ID else Folder.get_root()

    @staticmethod
    def find_by_location(location, name) -> Optional['Folder']:
        return Folder.query.filter_by(name=name, location=location).first()

    @staticmethod
    def find_by_path(path, fallback_to_id=False) -> Optional['Folder']:
        if path == '':
            return Folder.get_root()
        parent_loc, name = split_location(path)
        f = Folder.find_by_location(parent_loc, name)
        if f is None and fallback_to_id:
            try:
                return Folder.get_by_id(int(path))
            except ValueError:
                return None
        return f

    @staticmethod
    def find_first_existing(path: str) -> Optional['Folder']:
        """Finds the first existing folder for the given path.

        For example, if the folders

        a
        a/b

        exist, then:

        if path is a/b/c, returns a/b
        if path is a/b, returns a
        if path is a, returns the root folder
        if path is empty, returns None

        """
        while path:
            path, _ = split_location(path)
            d = Folder.find_by_path(path)
            if d:
                return d
        return None

    @staticmethod
    def get_all_in_path(root_path: str = '', filter_ids: Optional[Iterable[int]]=None) -> List['Folder']:
        """Gets all the folders under a path.

        :param root_path: Restricts the search to a specific folder.
        :param filter_ids: An optional iterable of document ids for filtering the folders.
               Must be non-empty if supplied.
        :return: A list of Folder objects.

        """
        q = Folder.query.filter_by(location=root_path)
        if filter_ids:
            q = q.filter(Folder.id.in_(filter_ids))
        return q.all()

    def is_root(self) -> bool:
        return self.id == -1

    def delete(self):
        assert self.is_empty
        db.session.delete(self)
        BlockAccess.query.filter_by(block_id=self.id).delete()
        Block.query.filter_by(type_id=blocktypes.FOLDER, id=self.id).delete()

    def rename(self, new_path: str) -> None:
        """Renames the folder, updating all the documents within.

        :param new_path: The new name for the folder.

        """

        old_name = self.path
        self.path = new_path

        # Rename contents
        docs_in_folder = DocEntry.query.filter(DocEntry.name.like(old_name + '/%')).all()  # type: List[DocEntry]
        for d in docs_in_folder:
            d.name = d.name.replace(old_name, new_path, 1)

        folders_in_folder = Folder.query.filter(
            (Folder.location == old_name) | (Folder.location.like(old_name + '/%'))).all()
        for f in folders_in_folder:
            f.location = f.location.replace(old_name, new_path, 1)

    @property
    def is_empty(self):
        q = Folder.query.filter_by(location=self.path)
        if db.session.query(q.exists()).scalar():
            return False
        q = DocEntry.query.filter(DocEntry.name.like(self.path + '/%'))
        return not db.session.query(q.exists()).scalar()

    @property
    def parent(self) -> Optional['Folder']:
        if self.is_root():
            return None
        return super().parent

    @property
    def path(self):
        return self.get_full_path()

    # noinspection PyMethodOverriding
    @path.setter
    def path(self, new_path: str):
        loc, name = split_location(new_path)
        self.location = loc
        self.name = name

    @property
    def path_without_lang(self):
        """Returns path without the language part.

        For folders, this is the same as the path itself.

        """
        return self.path

    def get_full_path(self) -> str:
        return join_location(self.location, self.name)

    def get_document(self, relative_path: str, create_if_not_exist=False, creator_group_id: int = None) -> Optional[
            DocEntry]:
        doc = DocEntry.query.filter_by(name=join_location(self.get_full_path(), relative_path)).first()
        if doc is not None:
            return doc
        if create_if_not_exist:
            rel_folder, _ = split_location(relative_path)
            Folder.create(join_location(self.get_full_path(), rel_folder), owner_group_id=creator_group_id,
                          commit=False)
            return DocEntry.create(join_location(self.get_full_path(), relative_path),
                                   owner_group_id=creator_group_id,
                                   title=relative_path)
        else:
            return None

    def get_all_documents(self, include_subdirs: bool = False) -> List[DocEntry]:

        query = DocEntry.query.filter(DocEntry.name.like(self.get_full_path() + '%'))

        if include_subdirs is False:
            query.filter(DocEntry.name.notlike(self.get_full_path() + '%' + os.sep + '%'))

        return query.all()


    @staticmethod
    def create(path: str, owner_group_id: int, title=None, commit=True, apply_default_rights=False) -> 'Folder':
        """Creates a new folder with the specified name. If the folder already exists, it is returned.

        :param title: The folder title.
        :param apply_default_rights: Whether to apply default rights from parents.
        :param commit: Whether to commit the changes.
        :param path: The name of the folder to be created.
        :param owner_group_id: The id of the owner group.
        :returns: The created or existing folder.

        """

        if '\0' in path:
            raise TimDbException('Folder name cannot contain null characters.')

        path = path.strip('/')

        if DocEntry.find_by_path(path):
            db.session.rollback()
            raise TimDbException('A document already exists at path {}'.format(path))

        # Root folder is special case
        if not path:
            return Folder.get_root()

        rel_path, rel_name = split_location(path)
        folder = Folder.query.filter_by(name=rel_name, location=rel_path).first()
        if folder is not None:
            return folder

        block_id = insert_block(title or rel_name, owner_group_id, blocktypes.FOLDER, commit=False).id

        # noinspection PyArgumentList
        f = Folder(id=block_id, name=rel_name, location=rel_path)
        db.session.add(f)

        # Make sure that the parent folder exists
        Folder.create(rel_path, owner_group_id, commit=False)

        if apply_default_rights:
            copy_default_rights(f.id, blocktypes.FOLDER, commit=False)

        if commit:
            db.session.commit()

        return f

    def to_json(self):
        return {**super().to_json(),
                'isFolder': True
                }
