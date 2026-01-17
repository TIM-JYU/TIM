from __future__ import annotations

import re
from heapq import heappush, heappop
from itertools import accumulate
from typing import Iterable, Generator, TYPE_CHECKING, Optional

import sqlalchemy
from sqlalchemy import select, Result, String
from sqlalchemy.dialects.postgresql import ARRAY
from sqlalchemy.orm import selectinload

from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
    DEFAULT_PREAMBLE_DOC,
)
from timApp.document.usercontext import UserContext
from timApp.document.validationresult import DoValidation
from timApp.document.viewcontext import default_view_ctx
from timApp.item.item import Item
from timApp.markdown.markdownconverter import expand_macros_info
from timApp.notification.notification import Notification
from timApp.timdb.sqa import db, run_sql
from timApp.user.usergroup import UserGroup
from timApp.util.utils import get_current_time, partition
from tim_common.utils import safe_parse_item_list

if TYPE_CHECKING:
    from timApp.document.translation.translation import Translation
    from timApp.document.docentry import DocEntry

GROUP_PREAMBLE_PREFIX = "preamble"
GROUP_PREAMBLE_PATTERN = re.compile(
    rf"^{GROUP_PREAMBLE_PREFIX}-(?P<group_name>.+?)-(?P<priority>\d+)$"
)


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

        # Templates don't have preambles. Other preambles don't have preambles (for now).
        if any(
            p == TEMPLATE_FOLDER_NAME or p == PREAMBLE_FOLDER_NAME for p in path_parts
        ):
            return []

        from timApp.auth.sessioninfo import user_context_with_logged_in_or_anon

        user_ctx = user_context_with_logged_in_or_anon()

        result = self._load_preamble_docs(paths, user_ctx, preamble_path_part)
        preamble_docs = []
        for de, tr in result:
            d = tr or de  # preamble either has the corresponding translation or not
            preamble_docs.append(d)
            d.document.ref_doc_cache = self.document.ref_doc_cache

            preamble_docs.extend(
                self._load_extra_preambles(d, user_ctx, preamble_path_part)
            )

            preamble_docs.extend(self._load_extra_group_preambles(d, user_ctx))

        return preamble_docs

    def _load_preamble_docs(
        self,
        doc_paths: list[str],
        user_ctx: UserContext,
        preamble_path_part: str,
    ) -> list[tuple["DocEntry", Optional["Translation"]]]:
        from timApp.document.docentry import DocEntry
        from timApp.document.translation.translation import Translation

        doc_paths_index_map = {path: i for i, path in enumerate(doc_paths)}
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
        doc_ids: set[int] = set()

        for de, tr in docs_q:  # type: DocEntry, Translation | None
            d = tr or de
            # Technically, we can have duplicates through aliases, so we need to dedupe here
            if d.id in doc_ids:
                continue
            doc_ids.add(d.id)
            path = d.path_without_lang
            if preamble_path_part not in path and not user_ctx.user.has_view_access(d):
                continue
            docs.append((de, tr))

        # The query may return the documents in a different order than requested
        # Therefore, we sort documents by the original path order
        docs.sort(key=lambda x: doc_paths_index_map[x[0].path])
        return docs

    def _load_extra_preambles(
        self,
        preamble_doc: DocInfo,
        user_ctx: UserContext,
        preamble_path_part: str,
    ) -> list[DocInfo]:
        result = []
        cur_doc_path = self.path_without_lang
        settings = preamble_doc.document.get_settings()

        extra_preambles = settings.extra_preambles()
        if not extra_preambles:
            return result

        macro_info = settings.get_macroinfo(default_view_ctx, user_ctx)
        macro_info.macro_map.update(
            {
                "ref_docid": self.id,
                "ref_docpath": cur_doc_path,
            }
        )

        if isinstance(extra_preambles, str):
            extra_preamble_doc_paths = safe_parse_item_list(
                expand_macros_info(extra_preambles, macro_info, ignore_errors=True)
            )
        else:
            extra_preamble_doc_paths = [
                expand_macros_info(ep, macro_info, ignore_errors=True)
                for ep in extra_preambles
            ]
        # Strip any extra spaces and remove any falsy values (empty strings) if they get evaluated as such
        # Also remove any self-references
        extra_preamble_doc_paths = list(
            dict.fromkeys(
                edp_t
                for edp in extra_preamble_doc_paths
                if (edp_t := edp.strip()) and edp_t != cur_doc_path
            )
        )
        # TODO: Should extraPreambles be recursive?
        extra_docs = self._load_preamble_docs(
            extra_preamble_doc_paths, user_ctx, preamble_path_part
        )
        for edr, etr in extra_docs:  # type: DocEntry, Translation | None
            ed = etr or edr
            result.append(ed)
            ed.document.ref_doc_cache = self.document.ref_doc_cache

        return result

    def _load_extra_group_preambles(
        self,
        preamble_doc: DocInfo,
        user_ctx: UserContext,
    ) -> list[DocInfo]:
        from timApp.document.docentry import DocEntry

        result = []
        settings = preamble_doc.document.get_settings()
        extra_group_preambles_folder = settings.extra_group_preambles_folder()

        if not extra_group_preambles_folder:
            return result

        # Normalize; the path is always assumed to be relative to the root
        extra_group_preambles_folder = extra_group_preambles_folder.strip("/")

        # If the current doc is in the extra group preambles folder, we don't need to load anything
        # to prevent recursion.
        if self.path.startswith(f"{extra_group_preambles_folder}/"):
            return result

        # Fetch all potential group preamble documents
        #  1. Get the groups that the current user belongs to
        #  2. Find all documents that have the path of "extra_group_preambles_folder/<group_name>-"
        #  3. Process the document paths
        #     - Check the doc path is of format <group_name>-<priority>
        # In general, we expect N(groups) << N(documents), so we separately fetch groups and construct
        # a custom OR query to find the potential group preamble documents.
        # This way we can make use of DocEntry.name being indexed.
        user_group_names: set[str] = set(
            run_sql(
                user_ctx.user.get_groups(
                    include_special=False, include_expired=False
                ).with_only_columns(UserGroup.name)
            )
            .scalars()
            .all()
        )
        group_preamble_candidates_condition = DocEntry.name.like(
            sqlalchemy.any_(
                sqlalchemy.cast(
                    [
                        f"{extra_group_preambles_folder}/{GROUP_PREAMBLE_PREFIX}-{group_name}-%"
                        for group_name in user_group_names
                    ],
                    ARRAY(String),
                )
            )
        )
        potential_group_preamble_doc_paths_query = select(DocEntry.name).filter(
            group_preamble_candidates_condition
        )
        potential_group_preamble_doc_paths = (
            run_sql(potential_group_preamble_doc_paths_query).scalars().unique()
        )

        group_preamble_docs_queue = []

        for doc_path in potential_group_preamble_doc_paths:  # type: str
            last_part = doc_path.rsplit("/", 1)[-1]
            match = GROUP_PREAMBLE_PATTERN.match(last_part)
            if not match:
                continue
            group_name = match.group("group_name")
            if group_name not in user_group_names:
                continue
            # We have a valid group preamble document
            # Add it to the list of group preamble docs by its priority
            # Note: Python's priority queue is a min-heap, so we use negative priority
            heappush(
                group_preamble_docs_queue, (-int(match.group("priority")), doc_path)
            )

        if not group_preamble_docs_queue:
            return result

        # Now, pick the preambles with the top priority
        final_group_preamble_paths = []
        priority, doc_path = heappop(group_preamble_docs_queue)
        final_group_preamble_paths.append(doc_path)
        # If there are multiple preambles with the same priority, we pick them all;
        # this is why we peek the element before popping it.
        while group_preamble_docs_queue:
            next_priority, doc_path = heappop(group_preamble_docs_queue)
            if next_priority != priority:
                break
            final_group_preamble_paths.append(doc_path)

        # The final list has the group preamble document paths
        # We can finally load the documents, but in reverse to ensure those with the higher priority apply last.
        group_preamble_docs = self._load_preamble_docs(
            final_group_preamble_paths[::-1],
            user_ctx,
            preamble_path_part="",  # Empty, because the check has already been done while fetching
        )
        for edr, etr in group_preamble_docs:  # type: DocEntry, Translation | None
            ed = etr or edr
            result.append(ed)
            ed.document.ref_doc_cache = self.document.ref_doc_cache

        return result

    def get_changelog_with_names(self, length=None, complete: bool = False):
        if not length:
            length = getattr(self, "changelog_length", 100)
        return self.document.get_changelog(length, complete)

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
        serialize_content = getattr(self, "serialize_content", False)
        metadata_info = getattr(getattr(self, "metadata", None), "info", None) or {}
        if not isinstance(metadata_info, dict):
            metadata_info = {}
        add_errors = metadata_info.get("route") == "manage" and serialize_content

        result = {**super().to_json(**kwargs), "isFolder": False}

        if serialize_content:
            do_validation = DoValidation.CHECK if add_errors else DoValidation.NONE
            result.update(
                {
                    "versions": self.get_changelog_with_names(),
                    "fulltext": self.document.export_markdown(
                        do_validation=do_validation
                    ),
                }
            )

            errors = metadata_info.get("errors", None)
            if add_errors and errors:
                result["errors"] = errors

        return result


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
