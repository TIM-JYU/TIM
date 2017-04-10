from enum import Enum

from documentmodel.randutils import hashfunc
from timdb.models.user import User


class PrintFormats(Enum):
    LATEX = 'latex'
    PDF = 'pdf'


class PrintSettings:

    def __init__(self, font_size: int = 12, margins: float = 2.5):
        """

        :param font_size:
        """
        self.settings = {}

        self.settings.pop('font_size', font_size)
        self.settings.pop('margins', margins)

    # def __init__(self, user: User):
    #    self.user = user
    #    f = user.get_personal_folder()
    #    self.print_settings_doc = f.get_document('Print Settings',
    #                                             create_if_not_exist=True,
    #                                             creator_group_id=user.get_personal_group().id).document)

    @property
    def hash_value(self) -> int:
        return hashfunc(','.join([pair for pair in self.settings]))
