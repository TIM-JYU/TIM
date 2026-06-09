from dataclasses import dataclass

from typing import cast
from timApp.document.document import Document
from timApp.document import docentry
from timApp.document.docentry import DocEntry, get_documents_in_folder
from timApp.folder.folder import Folder
from timApp.item.item import Item
from timApp.modules.asktim.llm_rule import LLMRule
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.auth.get_user_rights_for_item import (
    get_user_rights_for_item,
    UserItemRights,
)


@dataclass
class TimDatabase:
    @staticmethod
    def get_tim_document_by_id(doc_id: int) -> Document | None:
        """
        Returns a document corresponding to the given id.
        """
        doc_entry = TimDatabase.get_doc_entry_by_id(doc_id)
        if doc_entry:
            return doc_entry.document
        return None

    @staticmethod
    def get_tim_document_by_path(path: str) -> Document | None:
        doc = DocEntry.find_by_path(path)
        return doc.document if doc else None

    @staticmethod
    def get_tim_documents_by_path(path: str, recursive: bool = False) -> list[Document]:
        """
        Returns all documents in the given path recursively.
        If no documents are found, returns empty list.
        If path is a document the document is returned.
        If path is a folder all the documents in the folder are returned.
        If recursive is True, all subfolders are returned recursively.
        If path is neither a folder nor a document, returns empty list.
        """
        documents: list[Document] = []
        document = TimDatabase.get_tim_document_by_path(path)
        if document:
            documents.append(document)
            return documents

        doc_entries = docentry.get_documents(
            filter_folder=path, search_recursively=recursive
        )
        for d in doc_entries if doc_entries else []:
            documents.append(d.document)  # paragraphs -> .get_paragraphs()
        return documents

    @staticmethod
    def get_doc_entry_by_id(doc_id: int) -> DocEntry | None:
        entries = docentry.DocEntry.find_all_by_id(doc_id)
        if not entries:
            return None
        return entries[0]

    @staticmethod
    def check_rights(user_id: int, doc_id: int) -> UserItemRights | None:
        """
        Checks which rights the given user has for the given document or folder.
        """
        user = User.get_by_id(user_id)
        doc = Item.find_by_id(doc_id)
        if user and doc:
            rights = get_user_rights_for_item(doc, user)
            return rights
        else:
            return None

    @staticmethod
    def get_user_groups(groups: list[str]) -> list[UserGroup]:
        """Check if all the user groups exists and return them."""
        user_groups: list[UserGroup] = []
        for group in groups:
            g = UserGroup.get_by_name(group)
            if not g:
                raise Exception(f"Invalid user group: {group}")
            user_groups.append(g)
        return user_groups

    @staticmethod
    def validate_item_paths(
        user: User,
        paths: list[str],
        *,
        depth: int = 1,
        add_documents: bool = True,
    ) -> list[Item]:
        """
        Check if all the paths exist and that the user has access to them.
        If the path is a folder, check access to the documents directly under it.

        :param user: The user of which access to check.
        :param paths: The list of paths to check.
        :param depth: The depth to check.
                      If depth > 0, check the documents under folders.
        :param add_documents: Whether to add documents to the returned item list.
        :return: The list of items found.
        """
        items: list[Item] = []
        to_check: list[str] = []

        for path in paths:
            item = Item.find_by_path(path)
            if not item:
                raise Exception(f"Invalid item path: '{path}'")

            is_folder = isinstance(item, Folder)

            rights = get_user_rights_for_item(item, user)
            if not rights.get("owner") and not rights.get("manage"):
                raise Exception(f"Insufficient rights: '{path}'")

            if not is_folder:
                if add_documents:
                    items.append(item)
                continue

            items.append(item)
            folder: Folder = cast(Folder, item)

            # Check rights for the documents under this folder
            if depth > 0:
                docs = get_documents_in_folder(path)
                to_check.extend([d.name for d in docs])
                # Check rights for the folders under this folder
                if depth > 1:
                    folders = folder.get_all_folders()
                    to_check.extend([f.path for f in folders])

        # Check the rights for pending items
        if len(to_check) > 0:
            sub_items = TimDatabase.validate_item_paths(
                user, to_check, add_documents=False, depth=depth - 1
            )
            items.extend(sub_items)

        return items

    @staticmethod
    def api_key_valid_in_doc(key: LLMRule, doc_id: int) -> bool:
        """Check if the API key can be used in the given document."""
        if key.document_id > 0:
            raise ValueError("Not an API key")
        if len(key.paths) == 0:
            raise PermissionError("API key is not valid in any document.")

        for path in key.paths:
            item = Item.find_by_path(path)
            if not item:
                continue
            is_doc = isinstance(item, DocEntry)

            if is_doc:
                doc = cast(DocEntry, item)
                if doc.id == doc_id:
                    return True
                continue

            # Check if the document is this folder
            folder = cast(Folder, item)
            docs = folder.get_all_documents()
            # TODO: can probably be done more efficiently
            for doc_info in docs:
                if doc_info.document.doc_id == doc_id:
                    return True
        entry = TimDatabase.get_doc_entry_by_id(doc_id)
        if entry is None:
            raise ValueError(f"No document with ID {doc_id}")
        raise PermissionError(f"API key has no access to document '{entry.path}'")

