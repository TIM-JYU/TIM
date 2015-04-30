from collections import namedtuple

from contracts import new_contract


class DocIdentifier(namedtuple("DocIdentifier", "id hash")):
    """A document identifier consists of the id of the document and the version hash."""
    __slots__ = ()

    def __str__(self):
        return str(self.id) + ':' + str(self.hash)


new_contract('DocIdentifier', DocIdentifier)
