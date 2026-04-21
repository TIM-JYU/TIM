from dataclasses import dataclass

from sqlalchemy import select, delete

from timApp.document.document import Document
from timApp.document import docentry
from timApp.document.docentry import DocEntry
from timApp.item.item import Item
from timApp.modules.chattim.dbmodels import LLMRule, Policy, Usage
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.auth.get_user_rights_for_item import (
    get_user_rights_for_item,
    UserItemRights,
)
from timApp.user.usergroup import UserGroup


@dataclass
class TimDatabase:
    @staticmethod
    def identify_user(user_id: int) -> str:
        """
        Checks if the given user is a teacher or a student
        """
        user = User.get_by_id(user_id)
        if user:
            teacher = user.is_sisu_teacher
            if teacher:
                return "teacher"
            student = User.get_home_org_student_id(user)
            if student:
                return "student"
        return "user"  # TODO: better options

    @staticmethod
    def get_tim_document_by_id(doc_id: int) -> Document | None:
        """
        Returns a document corresponding to the given id.
        """
        doc_entry = docentry.DocEntry.find_all_by_id(doc_id)
        if doc_entry:
            return doc_entry[0].document  # paragraphs -> .get_paragraphs()
        return None

    @staticmethod
    def get_tim_documents_by_path(path: str) -> list[Document]:
        """
        Returns all documents in the given path recursively.
        If no documents are found, returns empty list.
        If path is a document the document is returned.
        If path is a folder all the documents in the folder and it's subfolders are returned recursively.
        If path is neither a folder nor a document, returns empty list.
        """
        documents = []
        doc = DocEntry.find_by_path(path)
        if doc:
            documents.append(doc.document)
            return documents

        doc_entries = docentry.get_documents(filter_folder=path)
        for d in doc_entries if doc_entries else []:
            documents.append(d.document)  # paragraphs -> .get_paragraphs()
        return documents

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
