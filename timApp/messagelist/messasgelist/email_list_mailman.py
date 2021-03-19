from dataclasses import dataclass
from typing import List

from mailmanclient import Client

from timApp.messagelist.messasgelist.email_list_tim import IEmailList, IEmailListManager

@dataclass
class EmailListManager(IEmailListManager):
    # VIESTIM: define new attributes, functions (and other things) in IEmailListManager first, then make necessary
    #  changes here. This naturally doesn't apply to completely implementation dependent details.
    def search_for_name(self, name: str) -> bool:
        pass

    @staticmethod
    def get_domains() -> List[str]:
        pass

    def _set_domains(self) -> None:
        pass



@dataclass
class EmailList(IEmailList):
    """Implementation of TIM's email list interface. Interacts with TIM's email list engine Mailman 3 via
    mailmanclient library.
    """

    # TODO: Arguments given to Client here are the same as in
    #  https://mailmanclient.readthedocs.io/en/latest/src/mailmanclient/docs/using.html
    #  Set correct values for contacting Mailman on it's server.
    _manager: Client = Client("http://localhost:9001/3.1", 'restadmin', "restpass")

    # VIESTIM: Would it be polite to return something as an indication how the operation went?
    def set_archive_type(self, archive: bool) -> None:
        pass

    def delete_email(self, email: str) -> None:
        pass

    def add_email(self, email: str) -> None:
        pass

    def check_name_availability(self, name_candidate: str) -> bool:
        pass
