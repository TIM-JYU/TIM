from datetime import timezone, datetime
from itertools import accumulate
from typing import List, Iterable, Generator, Tuple, Optional

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.documentmodel.document import Document
from timApp.documentmodel.specialnames import TEMPLATE_FOLDER_NAME, PREAMBLE_FOLDER_NAME, DEFAULT_PREAMBLE_DOC
from timApp.timdb.item import Item
from timApp.timdb.models.notification import NotificationType, Notification
from timApp.timdb.tim_models import db
from timApp.types import TranslationType


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
    def translations(self) -> List[TranslationType]:
        """Returns the translations of the document. NOTE: The list *includes* the document itself."""
        raise NotImplementedError

    @property
    def lang_id(self) -> str:
        raise NotImplementedError

    def update_last_modified(self):
        self.block.modified = datetime.now(tz=timezone.utc)

    def get_preamble_docs(self) -> List['DocInfo']:
        """Gets the list of preamble documents for this document.
        The first document in the list is nearest root.
        """
        if getattr(self, '_preamble_docs', None) is None:
            preamble_setting = self.document.get_own_settings().get('preamble', DEFAULT_PREAMBLE_DOC)
            self._preamble_docs = self._get_preamble_docs_impl(preamble_setting) if isinstance(preamble_setting, str) else []
        return self._preamble_docs

    def get_preamble_pars(self) -> Generator[DocParagraph, None, None]:
        return get_non_settings_pars_from_docs(self.get_preamble_docs())

    def _get_preamble_docs_impl(self, preamble_setting: str) -> List['DocInfo']:
        preamble_names = preamble_setting.split(',')
        path_parts = self.path_without_lang.split('/')
        paths = list(
            f'{p}{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{preamble_name.strip()}' for
            p in
            accumulate(part + '/' for part in path_parts[:-1]) for preamble_name in preamble_names)
        if not paths:
            return []

        path_index_map = dict((path, i) for i, path in enumerate(paths))

        # Templates don't have preambles.
        if any(p == TEMPLATE_FOLDER_NAME for p in path_parts):
            return []

        from timApp.timdb.models.docentry import DocEntry
        from timApp.timdb.models.translation import Translation
        result = db.session.query(DocEntry, Translation).filter(
            DocEntry.name.in_(paths)).outerjoin(Translation,
                                                (Translation.src_docid == DocEntry.id) & (
                                                    Translation.lang_id == self.lang_id)).all()  # type: List[Tuple[DocEntry, Optional[Translation]]]
        result.sort(key=lambda x: path_index_map[x[0].path])
        preamble_docs = []
        for de, tr in result:
            d = tr or de  # preamble either has the corresponding translation or not
            preamble_docs.append(d)
            d.document.ref_doc_cache = self.document.ref_doc_cache
        return preamble_docs

    def get_changelog_with_names(self, length=None):
        if not length:
            length = getattr(self, 'changelog_length', 100)
        return self.document.get_changelog(length)

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
