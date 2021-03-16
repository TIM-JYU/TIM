from dataclasses import dataclass

import mailmanclient as mailmanclient

from timApp.messagelist.messasgelist.email_list_tim import IEmailList, IEmailListManager


@dataclass
class EmailListManager(IEmailListManager):
    def search_for_name(self, name: str) -> bool:
        pass

    def get_domains(self) -> list[str]:
        pass

    def _set_domains(self) -> None:
        pass


@dataclass
class EmailList(IEmailList):
    """Implementation of TIM's email list interface. Interacts with TIM's email list engine Mailman 3 via
    mailmanclient library.
    """

    client = mailmanclient

    def set_archive_type(self, archive: bool) -> None:
        pass

    def delete_email(self, email: str) -> None:
        pass

    def add_email(self, email: str) -> None:
        pass

    def check_name_availability(self, name_candidate: str) -> bool:
        pass
