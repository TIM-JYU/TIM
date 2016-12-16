from documentmodel.document import Document
from timdb.item import Item
from timdb.models.block import Block
from timdb.tim_models import db
from utils import split_location, date_to_relative


class DocInfo(Item):
    """A base class for DocEntry and Translation."""
    @property
    def src_docid(self):
        """Returns the source document id in case of a translation or the document id itself otherwise."""
        return self.id

    @property
    def aliases(self):
        from timdb.models.docentry import DocEntry
        return DocEntry.find_all_by_id(self.src_docid)

    @property
    def document(self):
        """Returns the corresponding Document object."""
        return Document(self.id)

    @property
    def last_modified(self):
        return self.block.modified if self.block else None

    @property
    def translations(self):
        """Returns the translations of the document."""
        raise NotImplementedError

    @property
    def lang_id(self):
        raise NotImplementedError

    def has_translation(self, lang_id):
        for t in self.translations:
            if t.lang_id == lang_id:
                return True
        return False

    def add_alias(self, new_name, is_public):
        from timdb.models.docentry import DocEntry
        d = DocEntry(id=self.src_docid, name=new_name, public=is_public)
        db.session.add(d)

    def to_json(self):
        return {**super().to_json(),
                'isFolder': False
                }
