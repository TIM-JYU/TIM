from itertools import accumulate
from typing import List, Iterable, Generator

from sqlalchemy import func

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import Document
from timApp.timdb.item import Item

if False:
    from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.notification import NotificationType, Notification
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.tim_models import db


class DocInfo(Item):
    """A base class for DocEntry and Translation."""

    @property
    def path(self) -> str:
        raise NotImplementedError

    @property
    def path_without_lang(self) -> str:
        raise NotImplementedError

    @property
    def id(self) -> int:
        raise NotImplementedError

    @property
    def is_original_translation(self) -> bool:
        """Returns whether this object is the document from which other translated documents were created."""
        return self.id == self.src_docid

    @property
    def src_docid(self) -> int:
        """Returns the source document id in case of a translation or the document id itself otherwise."""
        return self.id

    @property
    def src_doc(self) -> 'DocInfo':
        """Returns the source document in case of a translation or the document itself otherwise."""
        if self.is_original_translation:
            return self
        from timApp.timdb.models.docentry import DocEntry
        return DocEntry.find_by_id(self.src_docid)

    @property
    def aliases(self):
        from timApp.timdb.models.docentry import DocEntry
        return DocEntry.find_all_by_id(self.src_docid)

    @property
    def document(self) -> Document:
        """Returns the corresponding Document object."""
        if getattr(self, '_doc', None) is None:
            self._doc = Document(self.id)
            self._doc.docinfo = self
        return self._doc

    @property
    def document_as_current_user(self) -> Document:
        if getattr(self, '_doc', None) is None:
            from timApp.sessioninfo import get_current_user_group
            self._doc = Document(self.id, modifier_group_id=get_current_user_group())
            self._doc.docinfo = self
        return self._doc

    @property
    def last_modified(self):
        return self.block.modified if self.block else None

    @property
    def translations(self) -> List['Translation']:
        """Returns the translations of the document. NOTE: The list *includes* the document itself."""
        raise NotImplementedError

    @property
    def lang_id(self) -> str:
        raise NotImplementedError

    def get_preamble_docs(self, preamble_name: str) -> List['DocEntry']:
        """Gets the list of preamble documents for this document.
        The first document in the list is nearest root.
        """
        if getattr(self, '_preamble_docs', None) is None:
            self._preamble_docs = self._get_preamble_docs_impl(preamble_name)
        return self._preamble_docs

    def _get_preamble_docs_impl(self, preamble_name: str) -> List['DocEntry']:
        path_parts = self.path_without_lang.split('/')
        paths = list(f'{p}Templates/preamble/{preamble_name}' for p in accumulate(part + '/' for part in path_parts[:-1]))
        if not paths:
            return []

        # Templates don't have preambles.
        if any(p == 'Templates' for p in path_parts):
            return []

        from timApp.timdb.models.docentry import DocEntry
        preamble_docs = DocEntry.query.filter(DocEntry.name.in_(paths)).order_by(func.length(DocEntry.name)).all()
        return preamble_docs

    def get_changelog_with_names(self, length=None):
        if not length:
            length = getattr(self, 'changelog_length', 100)
        changelog = self.document.get_changelog(length)
        for ver in changelog:
            ver['group'] = UserGroup.query.get(ver.pop('group_id')).name
        return changelog

    def get_notifications(self, notify_type: NotificationType) -> List[Notification]:
        q = Notification.query.filter_by(doc_id=self.id)
        if notify_type == NotificationType.CommentModified:
            q = q.filter_by(email_comment_modify=True)
        elif notify_type == NotificationType.CommentAdded:
            q = q.filter_by(email_comment_add=True)
        elif notify_type == NotificationType.DocModified:
            q = q.filter_by(email_doc_modify=True)
        else:
            assert False, 'Unknown NotificationType'
        return q.all()

    def has_translation(self, lang_id):
        for t in self.translations:
            if t.lang_id == lang_id:
                return True
        return False

    def add_alias(self, new_name, is_public):
        from timApp.timdb.models.docentry import DocEntry
        # noinspection PyArgumentList
        d = DocEntry(id=self.src_docid, name=new_name, public=is_public)
        db.session.add(d)

    def to_json(self):
        return {**super().to_json(),
                'isFolder': False,
                **({'versions': self.get_changelog_with_names(),
                    'fulltext': self.document.export_markdown()} if getattr(self, 'serialize_content', False) else {})
                }


def get_non_settings_pars_from_docs(docs: Iterable[DocInfo]) -> Generator[DocParagraph, None, None]:
    for d in docs:
        for p in d.document:
            if not p.is_setting() or p.is_area():
                yield p
