import os
from collections import OrderedDict
from enum import Enum
from typing import Optional

from documentmodel.randutils import hashfunc
from timdb.models.docentry import DocEntry
from timdb.models.user import User
from timdb.models.folder import Folder


class PrintFormat(Enum):
    LATEX = 'latex'
    PDF = 'pdf'

SETTINGS_DOC_NAME = 'PrintSettings'

class PrintSettings(object):

    def __init__(self, font_size: int = 12, margins: float = 2.5):
        """

        :param font_size:
        """
        self.font_size = font_size
        self.margins = margins

    # def __init__(self, user: User):
    #    self.user = user
    #    f = user.get_personal_folder()
    #    self.print_settings_doc = f.get_document('Print Settings',
    #                                             create_if_not_exist=True,
    #                                             creator_group_id=user.get_personal_group().id).document)

    @property
    def hash_value(self) -> str:
        return hashfunc(','.join([pair for pair in self.__dict__]))

    @staticmethod
    def _read_doc_settings(doc_entry: DocEntry) -> dict:
        doc_print_settings = doc_entry.document.get_settings().get_print_settings()
        return doc_print_settings

    @staticmethod
    def _read_user_settings(user: User) -> dict:
        print("Fetching users print settings...")
        user_home = user.get_personal_folder()
        print("Checking user home is @" + user_home.path)
        user_settings_doc = user_home.get_document(relative_path=SETTINGS_DOC_NAME.lower())
        print("Found file %s at user home: %r" % (SETTINGS_DOC_NAME, user_settings_doc is not None))
        if user_settings_doc is not None:
            return user_settings_doc.document.get_settings().get_print_settings()
        else:
            return {}

    @classmethod
    def read_settings(cls, doc_entry: DocEntry, user: User, user_priority: bool = True):
        """
        A factory function that reads the print settings for the given document for the given user.

        :param doc_entry: The document that is being printed
        :param user: The user that is printing the document.
        :param user_priority: Do the user settings overwrite other settings
        :return: A new PrintSettings object containing the settings
        """

        doc_settings = cls._read_doc_settings(doc_entry)
        user_settings = cls._read_user_settings(user)

        merged_settings = doc_settings.copy()
        merged_settings.update(user_settings)

        ret = """
            *********************
            Print settings for document
                %s
            with user
                %s
            *********************

                %s
        """ % (doc_entry.path, user.pretty_full_name, merged_settings)

        return ret
