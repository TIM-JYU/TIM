from itertools import accumulate

from flask import current_app
from sqlalchemy import tuple_, func

from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.models.block import Block
from timApp.timdb.tim_models import BlockAccess
from timApp.utils import split_location, date_to_relative


class Item:

    @property
    def block(self):
        # Relationships are not loaded when constructing an object with __init__.
        if not hasattr(self, '_block') or self._block is None:
            self._block = Block.query.get(self.id)
        return self._block

    @property
    def id(self):
        """Returns the item id."""
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
        return current_app.config['TIM_HOST'] + self.url_relative

    @property
    def url_relative(self):
        return '/view/' + self.path

    @property
    def location(self):
        folder, _ = split_location(self.path_without_lang)
        return folder

    @property
    def rights(self):
        from timApp.accesshelper import get_rights
        return get_rights(self.id)

    @property
    def title(self):
        if self.block is None:
            return 'All documents'
        if not self.block.description:
            return self.short_name
        return self.block.description

    @title.setter
    def title(self, value):
        self.block.description = value

    @property
    def owner(self):
        return self.block.owner if self.block else None

    @property
    def short_name(self):
        parts = self.path_without_lang.rsplit('/', 1)
        return parts[len(parts) - 1]

    @property
    def last_modified(self):
        return self.block.modified if self.block else None

    @property
    def parents_to_root(self):
        if not self.path_without_lang:
            return []
        path_parts = self.path_without_lang.split('/')
        paths = list(p[1:] for p in accumulate('/' + part for part in path_parts[:-1]))
        from timApp.timdb.models.folder import Folder
        if not paths:
            return [Folder.get_root()]
        path_tuples = [split_location(p) for p in paths]
        crumbs = Folder.query.filter(tuple_(Folder.location, Folder.name).in_(path_tuples)).order_by(func.length(Folder.location).desc()).all()
        crumbs.append(Folder.get_root())
        return crumbs

    @property
    def parent(self):
        folder = self.location
        from timApp.timdb.models.folder import Folder
        return Folder.find_by_path(folder) if folder else Folder.get_root()

    @property
    def public(self):
        return True

    def to_json(self):
        return {'name': self.short_name,
                'path': self.path,
                'title': self.title,
                'location': self.location,
                'id': self.id,
                'modified': date_to_relative(self.last_modified),
                'owner': self.owner,
                'rights': self.rights,
                'unpublished': self.block.is_unpublished() if self.block else False,
                'public': self.public
                }

    def get_relative_path(self, path: str):
        """Gets the item path relative to the given path.
        The item must be under the path; otherwise TimDbException is thrown.
        """
        path = path.strip('/')
        if not self.path.startswith(path + '/'):
            raise TimDbException('Cannot get relative path')
        return self.path.replace(path + '/', '', 1)

    @staticmethod
    def find_by_id(item_id):
        b = Block.query.get(item_id)
        if b:
            if b.type_id == blocktypes.DOCUMENT:
                from timApp.timdb.models.docentry import DocEntry
                return DocEntry.find_by_id(item_id)
            elif b.type_id == blocktypes.FOLDER:
                from timApp.timdb.models.folder import Folder
                return Folder.get_by_id(item_id)
            else:
                raise NotImplementedError
        return None


def copy_rights(source: Item, dest: Item, delete_existing=True):
    if delete_existing:
        dest.block.accesses.delete()
    for a in source.block.accesses:  # type: BlockAccess
        dest.block.accesses.append(
            BlockAccess(usergroup_id=a.usergroup_id,
                        type=a.type,
                        accessible_from=a.accessible_from,
                        accessible_to=a.accessible_to,
                        duration=a.duration,
                        duration_from=a.duration_from,
                        duration_to=a.duration_to,))
