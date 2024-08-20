from __future__ import annotations

from itertools import accumulate
from typing import TYPE_CHECKING

from flask import current_app
from sqlalchemy import tuple_, func, select
from sqlalchemy.orm import defaultload

from timApp.auth.auth_models import BlockAccess
from timApp.auth.get_user_rights_for_item import get_user_rights_for_item
from timApp.item.block import Block, BlockType
from timApp.item.blockrelevance import BlockRelevance
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import include_if_loaded, db, run_sql
from timApp.util.utils import split_location, date_to_relative, cached_property

if TYPE_CHECKING:
    from timApp.folder.folder import Folder
    from timApp.user.user import User


class ItemBase:
    """An item that can be assigned permissions."""

    @property
    def owners(self):
        return self.block.owners if self.block else None

    @property
    def block(self) -> Block:
        # Relationships are not loaded when constructing an object with __init__.
        if not hasattr(self, "_block") or self._block is None:
            self._block = db.session.get(Block, self.id, populate_existing=True)
        return self._block

    @property
    def id(self):
        """Returns the item id."""
        raise NotImplementedError

    @property
    def last_modified(self):
        return self.block.modified if self.block else None

    @property
    def parents(self):
        return self.block.parents

    @property
    def children(self):
        return self.block.children

    @property
    def relevance(self) -> BlockRelevance:
        return self.block.relevance if self.block else None


class Item(ItemBase):
    """An item that exists in the TIM directory hierarchy. Currently :class:`~.Folder` and :class:`~.DocInfo`."""

    @property
    def id(self):
        raise NotImplementedError

    @property
    def path(self):
        """Returns the Document path, including the language part in case of a translation."""
        raise NotImplementedError

    @property
    def path_without_lang(self):
        """Returns the Document path without the language part in case of a translation."""
        raise NotImplementedError

    @property
    def url(self):
        return current_app.config["TIM_HOST"] + self.url_relative

    def get_url_for_view(self, name: str):
        return f'{current_app.config["TIM_HOST"]}/{name}/{self.path}'

    def get_relative_url_for_view(self, name: str):
        return f"/{name}/{self.path}"

    @property
    def url_relative(self):
        return "/view/" + self.path

    @property
    def location(self):
        folder, _ = split_location(self.path_without_lang)
        return folder

    @property
    def title(self):
        if self.block is None:
            return "All documents"
        if not self.block.description:
            return self.short_name
        return self.block.description

    @title.setter
    def title(self, value):
        self.block.description = value

    @property
    def short_name(self):
        parts = self.path_without_lang.rsplit("/", 1)
        return parts[len(parts) - 1]

    def parents_to_root(self, include_root=True, eager_load_groups=False):
        if not self.path_without_lang:
            return []
        path_tuples = self.parent_paths()
        from timApp.folder.folder import Folder

        if not path_tuples:
            return [Folder.get_root()]

        # TODO: Add an option whether to load relevance eagerly or not;
        #  currently eager by default is better to speed up search cache processing
        #  and it doesn't slow down other code much.
        crumbs_stmt = (
            select(Folder)
            .filter(tuple_(Folder.location, Folder.name).in_(path_tuples))
            .order_by(func.length(Folder.location).desc())
            .options(defaultload(Folder._block).joinedload(Block.relevance))
        )
        if eager_load_groups:
            crumbs_stmt = crumbs_stmt.options(
                defaultload(Folder._block)
                .selectinload(Block.accesses)
                .joinedload(BlockAccess.usergroup)
            )
        crumbs = run_sql(crumbs_stmt).scalars().all()
        if include_root:
            crumbs.append(Folder.get_root())
        return crumbs

    def parent_paths(self) -> list[tuple[str, str]]:
        path_parts = self.path_without_lang.split("/")
        paths = list(p[1:] for p in accumulate("/" + part for part in path_parts[:-1]))
        return [split_location(p) for p in paths]

    @cached_property
    def parents_to_root_eager(self):
        return self.parents_to_root(eager_load_groups=True)

    @property
    def parent(
        self,
    ) -> (
        Folder
    ):  # TODO rename this to parent_folder to distinguish better from "parents" attribute
        folder = self.location
        from timApp.folder.folder import Folder

        return Folder.find_by_path(folder) if folder else Folder.get_root()

    @property
    def public(self):
        return True

    def to_json(self, curr_user: User | None = None):
        if curr_user is None:
            from timApp.auth.sessioninfo import get_current_user_object

            curr_user = get_current_user_object()
        return {
            "name": self.short_name,
            "path": self.path,
            "title": self.title,
            "location": self.location,
            "id": self.id,
            "modified": date_to_relative(self.last_modified)
            if self.last_modified
            else None,
            "owners": self.owners,
            "rights": get_user_rights_for_item(self, curr_user),
            "unpublished": self.block.is_unpublished() if self.block else False,
            "public": self.public,
            # We only add tags if they've already been loaded.
            **include_if_loaded("tags", self.block),
            **include_if_loaded("relevance", self.block),
        }

    def get_relative_path(self, path: str):
        """Gets the item path relative to the given path.
        The item must be under the path; otherwise TimDbException is thrown.
        """
        path = path.strip("/")
        if not self.path.startswith(path + "/"):
            raise TimDbException("Cannot get relative path")
        return self.path.replace(path + "/", "", 1)

    @staticmethod
    def find_by_id(item_id: int):
        b = db.session.get(Block, item_id)
        if b:
            if b.type_id == BlockType.Document.value:
                from timApp.document.docentry import DocEntry

                return DocEntry.find_by_id(item_id)
            elif b.type_id == BlockType.Folder.value:
                from timApp.folder.folder import Folder

                return Folder.get_by_id(item_id)
            else:
                raise NotImplementedError
        return None

    @staticmethod
    def find_by_path(path: str, fallback_to_id: bool = False) -> Item | None:
        """
        Finds an item by path. If the item is not found, None is returned.

        :param path: The path of the item to find.
        :param fallback_to_id: If True, the path is treated as an ID if the item is not found by path.
        :return: The item, or None if not found.
        """
        from timApp.document.docentry import DocEntry

        doc = DocEntry.find_by_path(path, fallback_to_id)
        if doc:
            return doc

        from timApp.folder.folder import Folder

        folder = Folder.find_by_path(path, fallback_to_id)
        if folder:
            return folder

        return None
