from dataclasses import dataclass
from timApp.document.document import Document
from timApp.document import docentry
from timApp.item.item import Item
from timApp.user.user import User
from timApp.auth.get_user_rights_for_item import (
    get_user_rights_for_item,
    UserItemRights,
)


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
        Returns a list of documents corresponding to the given path.
        """
        doc_entries = docentry.get_documents(filter_folder=path)
        documents = []
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

    @staticmethod
    def fetch_item_by_path(item_path: str) -> Item | None:
        """
        Gets item by path.
        """
        return Item.find_by_path(item_path)

    @staticmethod
    def check_rights_per_item(user_id: int, item: Item) -> UserItemRights | None:
        user = User.get_by_id(user_id)

        if not user:
            return None

        rights = get_user_rights_for_item(item, user)
        return rights
