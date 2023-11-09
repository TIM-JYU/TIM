from __future__ import annotations

from itertools import accumulate
from typing import Iterable, Generator, TYPE_CHECKING

from sqlalchemy import select, Result
from sqlalchemy.orm import selectinload

from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
    DEFAULT_PREAMBLE_DOC,
)
from timApp.document.viewcontext import default_view_ctx
from timApp.item.item import Item
from timApp.markdown.markdownconverter import expand_macros_info
from timApp.notification.notification import Notification
from timApp.timdb.sqa import db, run_sql
from timApp.util.utils import get_current_time, partition
from tim_common.utils import safe_parse_item_list

if TYPE_CHECKING:
    from timApp.document.translation.translation import Translation


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
    def src_doc(self) -> DocInfo:
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
        if getattr(self, "_doc", None) is None:
            self._doc = Document(self.id)
            self._doc.docinfo = self
        return self._doc

    @property
    def document_as_current_user(self) -> Document:
        if getattr(self, "_doc", None) is None:
            from timApp.auth.sessioninfo import get_current_user_group

            self._doc = Document(self.id, modifier_group_id=get_current_user_group())
            self._doc.docinfo = self
        return self._doc

    @property
    def last_modified(self):
        return self.block.modified if self.block else None

    @property
    def translations(self) -> list[Translation]:
        """Returns the translations of the document. NOTE: The list *includes* the document itself."""
        raise NotImplementedError

    @property
    def lang_id(self) -> str | None:
        raise NotImplementedError

    def update_last_modified(self) -> None:
        self.block.modified = get_current_time()

    def get_preamble_docs(self) -> list[DocInfo]:
        """Gets the list of preamble documents for this document.
        The first document in the list is the nearest root.
        """
        if getattr(self, "_preamble_docs", None) is None:
            preamble_setting = self.document.get_own_settings().get(
                "preamble", DEFAULT_PREAMBLE_DOC
            )
            self._preamble_docs = (
                self._get_preamble_docs_impl(preamble_setting)
                if isinstance(preamble_setting, str)
                else []
            )
        return self._preamble_docs

    def get_preamble_pars_with_class(self, class_names: list[str]):
        """
        Get all preamble pars with any of the given classes.

        :param class_names: Class names.
        :return: Filtered pars from the preamble document.
        """
        return get_pars_with_class_from_docs(self.get_preamble_docs(), class_names)

    def get_preamble_pars(self) -> Generator[DocParagraph, None, None]:
        return get_original_text_pars_from_docs(self.get_preamble_docs())

    def _get_preamble_docs_impl(self, preamble_setting: str) -> list[DocInfo]:
        preamble_names = preamble_setting.split(",")
        doc_path = self.path_without_lang
        path_parts = doc_path.split("/")
        preamble_path_part = f"/{PREAMBLE_FOLDER_NAME}/"

        # An absolute path begins with "/" and "/preambles/" appears in it.
        # If the conditions are met, then proceed as in the relative preamble.
        def absolute_path(variable: str) -> bool:
            variable = variable.strip()
            return variable.startswith("/")

        # These two lists are mutually exclusive to avoid if statements.
        absolute_path_parts, relative_path_parts = partition(
            absolute_path, preamble_names
        )

        paths = list(
            f"{p}{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{preamble_name.strip()}"
            for p in accumulate(part + "/" for part in path_parts[:-1])
            for preamble_name in relative_path_parts
        )

        for preamble_name in absolute_path_parts:
            if preamble_path_part in preamble_name:
                preamble_path_parts = preamble_name.split("/")[1:-2]
                preamble_name = preamble_name.split("/")[-1]
                paths.extend(
                    f"{p}{PREAMBLE_FOLDER_NAME}/{preamble_name.strip()}"
                    for p in accumulate(part + "/" for part in preamble_path_parts)
                )
            else:
                paths.append(preamble_name[1:])

        # Remove duplicates and then self-reference
        paths = list(dict.fromkeys(paths))
        if doc_path in paths:
            paths.remove(doc_path)

        if not paths:
            return []

        path_index_map = {path: i for i, path in enumerate(paths)}

        # Templates don't have preambles. Other preambles don't have preambles (for now).
        if any(
            p == TEMPLATE_FOLDER_NAME or p == PREAMBLE_FOLDER_NAME for p in path_parts
        ):
            return []

        from timApp.document.docentry import DocEntry
        from timApp.document.translation.translation import Translation
        from timApp.auth.sessioninfo import user_context_with_logged_in_or_anon

        user_ctx = user_context_with_logged_in_or_anon()

        def get_docs(doc_paths: list[str]) -> list[tuple[DocEntry, Translation | None]]:
            docs_q: Result[tuple[DocEntry, Translation | None]] = run_sql(
                select(DocEntry, Translation)
                .select_from(DocEntry)
                .filter(DocEntry.name.in_(doc_paths))
                .outerjoin(
                    Translation,
                    (Translation.src_docid == DocEntry.id)
                    & (Translation.lang_id == self.lang_id),
                )
            )

            docs = []

            for de, tr in docs_q:  # type: DocEntry, Translation | None
                d = tr or de
                path = d.path_without_lang
                if preamble_path_part not in path and not user_ctx.user.has_view_access(d):
                    continue
                docs.append((de, tr))

            return docs

        result = get_docs(paths)
        result.sort(key=lambda x: path_index_map[x[0].path])
        preamble_docs = []
        for de, tr in result:
            d = tr or de  # preamble either has the corresponding translation or not
            preamble_docs.append(d)
            d.document.ref_doc_cache = self.document.ref_doc_cache

            settings = d.document.get_settings()
            extra_preambles = settings.extra_preambles()
            if extra_preambles:
                macro_info = settings.get_macroinfo(default_view_ctx, user_ctx)
                macro_info.macro_map.update(
                    {
                        "ref_docid": self.id,
                        "ref_docpath": doc_path,
                    }
                )

                if isinstance(extra_preambles, str):
                    extra_preamble_doc_paths = safe_parse_item_list(
                        expand_macros_info(
                            extra_preambles, macro_info, ignore_errors=True
                        )
                    )
                else:
                    extra_preamble_doc_paths = [
                        expand_macros_info(ep, macro_info, ignore_errors=True)
                        for ep in extra_preambles
                    ]
                # Strip any extra spaces and remove any falsy values (empty strings) if they get evaluated as such
                # Also remove any self-references
                extra_preamble_doc_paths = list(
                    {
                        edp_t
                        for edp in extra_preamble_doc_paths
                        if (edp_t := edp.strip()) and edp_t != doc_path
                    }
                )
                # TODO: Should extraPreambles be recursive?
                extra_docs = get_docs(extra_preamble_doc_paths)
                for edr, etr in extra_docs:
                    ed = etr or edr
                    preamble_docs.append(ed)
                    ed.document.ref_doc_cache = self.document.ref_doc_cache

        return preamble_docs

    def get_changelog_with_names(self, length=None):
        if not length:
            length = getattr(self, "changelog_length", 100)
        return self.document.get_changelog(length)

    def get_notifications(self, condition) -> list[Notification]:
        items = set()
        for a in self.aliases:
            items.update(a.parents_to_root())
        items.add(self)
        from timApp.user.user import User

        stmt = (
            select(Notification)
            .options(selectinload(Notification.user).selectinload(User.groups))
            .filter(Notification.block_id.in_([f.id for f in items]))
        )
        stmt = stmt.filter(condition)
        return run_sql(stmt).scalars().all()

    def has_translation(self, lang_id):
        for t in self.translations:
            if t.lang_id == lang_id:
                return True
        return False

    def add_alias(self, new_name, is_public):
        from timApp.document.docentry import DocEntry

        d = DocEntry(id=self.src_docid, name=new_name, public=is_public)
        db.session.add(d)

    def to_json(self, **kwargs):
        return {
            **super().to_json(**kwargs),
            "isFolder": False,
            **(
                {
                    "versions": self.get_changelog_with_names(),
                    "fulltext": self.document.export_markdown(),
                }
                if getattr(self, "serialize_content", False)
                else {}
            ),
        }


def get_original_text_pars_from_docs(
    docs: Iterable[DocInfo],
) -> Generator[DocParagraph, None, None]:
    """
    Get all non-settings original content paragraphs from the documents.

    The following paragraphs are excluded:
    - Any setting paragraphs => not visible
    - Any area paragraphs => not visible
    - Any paragraphs that were copied from another preamble => not part of the original document

    :param docs: Document from which to get the paragraphs.
    :return: Original text paragraphs.
    """
    for d in docs:
        for p in d.document:
            if p.from_preamble():
                continue
            if not p.is_setting() or p.is_area():
                yield p


def get_pars_with_class_from_docs(
    docs: Iterable[DocInfo], class_names: list[str]
) -> Generator[DocParagraph, None, None]:
    """
    Get all non-settings original content paragraphs that have the given class.

    :param docs: Document.
    :param class_names: Class name list.
    :return: Pars that have any of the filtering class names.
    """
    for p in get_original_text_pars_from_docs(docs):
        classes = p.classes
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
        trash_path = f"{destination.path}/{short_name}"
        if not Folder.find_by_path(trash_path) and not DocEntry.find_by_path(
            trash_path
        ):
            break
        attempt += 1
        short_name = f"{item.short_name}_{attempt}"
    return trash_path
