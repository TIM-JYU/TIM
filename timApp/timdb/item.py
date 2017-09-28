from flask import current_app

from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.models.block import Block
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
        crumbs = []
        p = self.parent
        while p:
            crumbs.append(p)
            p = p.parent
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
