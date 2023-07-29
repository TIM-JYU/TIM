from __future__ import annotations

from typing import Iterable, Any, TYPE_CHECKING

from sqlalchemy import true, and_, select, delete, UniqueConstraint, ForeignKey
from sqlalchemy.orm import mapped_column, Mapped, relationship

from timApp.auth.auth_models import BlockAccess
from timApp.document.docentry import DocEntry, get_documents
from timApp.document.docinfo import DocInfo
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME
from timApp.folder.createopts import FolderCreationOptions
from timApp.item.block import Block, insert_block, copy_default_rights, BlockType
from timApp.item.item import Item
from timApp.timdb.exceptions import ItemAlreadyExistsException
from timApp.timdb.sqa import db
from timApp.timdb.types import DbModel
from timApp.user.usergroup import UserGroup
from timApp.util.utils import split_location, join_location, relative_location

if TYPE_CHECKING:
    from timApp.user.user import User

ROOT_FOLDER_ID = -1


class Folder(DbModel, Item):
    """Represents a folder in the directory hierarchy."""

    id: Mapped[int] = mapped_column(ForeignKey("block.id"), primary_key=True)
    """Folder identifier."""

    name: Mapped[str]
    """Folder name (last part of path)."""

    location: Mapped[str]
    """Folder location (first parts of the path)."""

    __table_args__ = (UniqueConstraint("name", "location", name="folder_uc"),)

    _block: Mapped[Block] = relationship(back_populates="folder", lazy="joined")

    @staticmethod
    def get_root() -> Folder:
        return Folder(id=ROOT_FOLDER_ID, name="", location="")

    @staticmethod
    def get_by_id(fid) -> Folder | None:
        return (
            db.session.get(Folder, fid) if fid != ROOT_FOLDER_ID else Folder.get_root()
        )

    @staticmethod
    def find_by_location(location, name) -> Folder | None:
        return (
            db.session.execute(select(Folder).filter_by(name=name, location=location))
            .scalars()
            .first()
        )

    @staticmethod
    def find_by_path(path, fallback_to_id=False) -> Folder | None:
        if path == "":
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
    def find_first_existing(path: str) -> Folder | None:
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
    def get_all_in_path(
        root_path: str = "",
        filter_ids: Iterable[int] | None = None,
        recurse=False,
    ) -> list[Folder]:
        """Gets all the folders under a path.

        :param recurse: Whether to search recursively.
        :param root_path: Restricts the search to a specific folder.
        :param filter_ids: An optional iterable of document ids for filtering the folders.
               Must be non-empty if supplied.
        :return: A list of Folder objects.

        """
        f_filter = Folder.location == root_path
        if recurse:
            f_filter = (
                (f_filter | Folder.location.startswith(root_path + "/"))
                if root_path
                else true()
            )
        stmt = select(Folder).filter(f_filter)
        if filter_ids:
            stmt = stmt.filter(Folder.id.in_(filter_ids))
        return db.session.execute(stmt).scalars().all()

    def is_root(self) -> bool:
        return self.id == -1

    def delete(self):
        assert self.is_empty
        db.session.delete(self)
        db.session.execute(delete(BlockAccess).where(BlockAccess.block_id == self.id))
        db.session.execute(
            delete(Block).where(
                (Block.type_id == BlockType.Folder.value) & (Block.id == self.id)
            )
        )

    def rename(self, new_name: str):
        assert "/" not in new_name
        old_path = self.path
        self.name = new_name
        self.rename_content(old_path, self.path)

    def rename_path(self, new_path: str) -> None:
        """Renames the folder, updating all the documents within.

        :param new_path: The new name for the folder.

        """

        old_path = self.path
        self.path = new_path
        self.rename_content(old_path, new_path)

    def rename_content(self, old_path: str, new_path: str):
        """Renames contents of the folder."""
        docs_in_folder: list[DocEntry] = (
            db.session.execute(
                select(DocEntry).filter(DocEntry.name.like(old_path + "/%"))
            )
            .scalars()
            .all()
        )
        for d in docs_in_folder:
            d.name = d.name.replace(old_path, new_path, 1)

        folders_in_folder = (
            db.session.execute(
                select(Folder).filter(
                    (Folder.location == old_path)
                    | (Folder.location.like(old_path + "/%"))
                )
            )
            .scalars()
            .all()
        )
        for f in folders_in_folder:
            f.location = f.location.replace(old_path, new_path, 1)

    @property
    def is_empty(self):
        stmt = select(Folder.id).filter_by(location=self.path)
        if db.session.execute(stmt.limit()).first():
            return False
        stmt = select(DocEntry.id).filter(DocEntry.name.like(self.path + "/%"))
        return not db.session.execute(stmt.limit(1)).first()

    @property
    def parent(self) -> Folder | None:
        if self.is_root():
            return None
        return super().parent

    @property
    def path(self):
        return self.get_full_path()

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

    @property
    def block(self) -> Block | None:
        """Overridden for optimization: root folder does not have a db entry, so we won't try to query for it."""
        if self.is_root():
            return None
        return Item.block.fget(self)

    def get_full_path(self) -> str:
        return join_location(self.location, self.name)

    def get_document(
        self, relative_path: str, create_if_not_exist=False, creator_group=None
    ) -> None | DocEntry:
        doc = (
            db.session.execute(
                select(DocEntry).filter_by(
                    name=join_location(self.get_full_path(), relative_path)
                )
            )
            .scalars()
            .first()
        )
        if doc is not None:
            return doc
        if create_if_not_exist:
            rel_folder, short_name = split_location(relative_path)
            Folder.create(
                join_location(self.get_full_path(), rel_folder),
                owner_groups=creator_group,
            )
            return DocEntry.create(
                join_location(self.get_full_path(), relative_path),
                owner_group=creator_group,
                title=short_name,
            )
        else:
            return None

    def get_all_documents(
        self,
        relative_paths: list[str] = None,
        include_subdirs: bool = False,
        custom_filter: Any = None,
        query_options: Any = None,
        filter_user: User | None = None,
    ) -> list[DocInfo]:
        if relative_paths is not None:
            include_subdirs = True
            paths = [
                join_location(self.get_full_path(), path) for path in relative_paths
            ]
            if custom_filter is None:
                custom_filter = DocEntry.name.in_(paths)
            else:
                custom_filter = and_(custom_filter, DocEntry.name.in_(paths))
        return get_documents(
            include_nonpublic=True,
            filter_folder=self.get_full_path(),
            search_recursively=include_subdirs,
            custom_filter=custom_filter,
            query_options=query_options,
            filter_user=filter_user,
        )

    def get_all_folders(self) -> list[Folder]:
        return Folder.get_all_in_path(self.path)

    def relative_path(self, item: Item) -> str:
        return relative_location(item.path, self.path)

    @staticmethod
    def create(
        path: str,
        owner_groups: list[UserGroup] | UserGroup | None = None,
        title=None,
        creation_opts: FolderCreationOptions = FolderCreationOptions(),
    ) -> Folder:
        """Creates a new folder with the specified name. If the folder already exists, it is returned.

        :param title: The folder title.
        :param creation_opts: Additional folder creation options.
        :param path: The name of the folder to be created.
        :param owner_groups: The owner group.
        :returns: The created or existing folder.

        """

        path = path.strip("/")

        if DocEntry.find_by_path(path):
            db.session.rollback()
            raise ItemAlreadyExistsException(
                f"A document already exists at path {path}"
            )

        # Root folder is special case
        if not path:
            return Folder.get_root()

        rel_path, rel_name = split_location(path)
        folder = (
            db.session.execute(
                select(Folder).filter_by(name=rel_name, location=rel_path)
            )
            .scalars()
            .first()
        )
        if folder is not None:
            return folder

        # Make sure that the parent folder exists
        p_f = Folder.create(rel_path, owner_groups)

        # Templates is a special folder, so it should have the same owner as its parent,
        # except if we're in users folder.
        owner_groups = (
            p_f.owners
            if creation_opts.get_templates_rights_from_parent
            and rel_name == TEMPLATE_FOLDER_NAME
            and rel_path != "users"
            else owner_groups
        )
        if owner_groups is None:
            owner_groups = []
        owner_groups = (
            owner_groups if isinstance(owner_groups, list) else [owner_groups]
        )
        block = insert_block(
            BlockType.Folder,
            title or rel_name,
            owner_groups,
        )

        f = Folder(_block=block, name=rel_name, location=rel_path)
        db.session.add(f)

        if creation_opts.apply_default_rights:
            copy_default_rights(f, BlockType.Folder, owners_to_skip=owner_groups)

        return f

    def to_json(self):
        return {**super().to_json(), "isFolder": True}


def path_includes(longer_path: str, shorter_path: str):
    longer_path_stripped = longer_path.strip("/")
    shorter_path_stripped = shorter_path.strip("/")
    return (longer_path_stripped + "/").startswith(shorter_path_stripped + "/")
