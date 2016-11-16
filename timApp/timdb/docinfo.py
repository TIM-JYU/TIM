from documentmodel.document import Document
from utils import split_location, date_to_relative


class DocInfo:
    """A base class for DocEntry and Translation."""

    @property
    def block(self):
        """Returns the Block object that corresponds to this DocInfo."""
        raise NotImplementedError

    @property
    def id(self):
        """Returns the Document id."""
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
    def document(self):
        """Returns the corresponding Document object."""
        return Document(self.id)

    @property
    def location(self):
        folder, _ = split_location(self.path_without_lang)
        return folder

    @property
    def rights(self):
        from routes.accesshelper import get_rights
        return get_rights(self.id)

    @property
    def title(self):
        return self.short_name or 'All documents'

    @property
    def short_name(self):
        parts = self.path_without_lang.rsplit('/', 1)
        return parts[len(parts) - 1]

    @property
    def last_modified(self):
        return self.document.get_last_modified()

    @property
    def parent(self):
        folder = self.location
        from timdb.models.folder import Folder
        return Folder.find_by_path(folder) if folder else Folder(id=-1)

    def to_json(self):
        return {'name': self.short_name,
                'path': self.path,
                'title': self.title,
                'location': self.location,
                'id': self.id,
                'modified': date_to_relative(self.last_modified),
                'isFolder': False,
                'owner': self.block.owner if self.block else None,
                'rights': self.rights,
                'unpublished': self.block.is_unpublished() if self.block else False
                }
