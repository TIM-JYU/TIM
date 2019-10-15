from itertools import accumulate
from typing import List, Iterable, Generator, Tuple, Optional

from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME, PREAMBLE_FOLDER_NAME, DEFAULT_PREAMBLE_DOC
from timApp.item.item import Item
from timApp.notification.notification import NotificationType, Notification
from timApp.timdb.sqa import db
from timApp.timtypes import TranslationType
from timApp.util.utils import get_current_time


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
        from timApp.document.docentry import DocEntry
        return DocEntry.find_by_id(self.src_docid)

    @property
    def aliases(self):
        from timApp.document.docentry import DocEntry
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
            from timApp.auth.sessioninfo import get_current_user_group
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
        self.block.modified = get_current_time()

    def get_preamble_docs(self) -> List['DocInfo']:
        """Gets the list of preamble documents for this document.
        The first document in the list is nearest root.
        """
        if getattr(self, '_preamble_docs', None) is None:
            preamble_setting = self.document.get_own_settings().get('preamble', DEFAULT_PREAMBLE_DOC)
            self._preamble_docs = self._get_preamble_docs_impl(preamble_setting) if isinstance(preamble_setting, str) else []
        return self._preamble_docs

    def get_preamble_pars_with_class(self, class_names: List[str]):
        """
        Get all preamble pars with any of the given classes.
        :param class_names: Class names.
        :return: Filtered pars from the preamble document.
        """
        return get_pars_with_class_from_docs(self.get_preamble_docs(), class_names)

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

        from timApp.document.docentry import DocEntry
        from timApp.document.translation.translation import Translation
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

    def get_notifications(self, condition) -> List[Notification]:
        items = set()
        for a in self.aliases:
            items.update(a.parents_to_root())
        items.add(self)
        q = Notification.query.filter(Notification.doc_id.in_([f.id for f in items]))
        q = q.filter(condition)
        return q.all()

    def has_translation(self, lang_id):
        for t in self.translations:
            if t.lang_id == lang_id:
                return True
        return False

    def add_alias(self, new_name, is_public):
        from timApp.document.docentry import DocEntry
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


def get_pars_with_class_from_docs(docs: Iterable[DocInfo], class_names: List[str]) -> Generator[DocParagraph, None, None]:
    """
    Loads all non-settings pars that have the given class.
    :param docs: Document.
    :param class_names: Class name list.
    :return: Pars that have any of the filtering class names.
    """
    for p in get_non_settings_pars_from_docs(docs):
        classes = p.get_attr("classes")
        if classes:
            for class_name in class_names:
               if class_name in classes:
                    yield p


def move_document(d: DocInfo, destination):
    aliases = d.aliases
    for a in aliases[1:]:
        db.session.delete(a)
    first_alias = aliases[0]
    first_alias.name = find_free_name(destination, first_alias)
    first_alias.public = True


def find_free_name(destination, item: Item):
    from timApp.document.docentry import DocEntry
    from timApp.folder.folder import Folder
    short_name = item.short_name
    attempt = 0
    while True:
        trash_path = f'{destination.path}/{short_name}'
        if not Folder.find_by_path(trash_path) and not DocEntry.find_by_path(trash_path):
            break
        attempt += 1
        short_name = f'{item.short_name}_{attempt}'
    return trash_path
