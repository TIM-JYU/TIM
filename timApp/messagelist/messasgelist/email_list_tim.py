from typing import Optional


class IEmailListManager:
    """This interface represents the functionality TIM shows from it's chosen email list management system (e.g.
    Mailman) to it's users.
    """

    def _set_domains(self) -> None:
        """In initialization the email list manager has to know what domains it can use, so the user can choose the
        proper one (in case there are multiple). No domain means either a configuration error somewhere else in the
        system or the organisation is not using the email list functionality at all.

        Domains are configured by appropriate personel, no user should get to just use any domain they wish.

        If domains are found, they are placed in the variable domains.
        """
        raise NotImplementedError

    def get_domains(self) -> list[str]:
        """Return all possible domains.

        :return a list of domains.
        """
        raise NotImplementedError

    def search_for_name(self, name: str) -> bool:
        raise NotImplementedError


class IEmailList:
    """TIM's email list interface. Defines functionality which TIM promises to it's users when they use email list
    functionality.
    """
    list_manager: IEmailListManager
    list_name: str
    domains: Optional[str]
    list_domain: str
    archive: bool

    def __init__(self):
        pass

    def check_name_availability(self, name_candidate: str) -> bool:
        """Check availability of a proposed mailing list name.

        :param name_candidate:

        :return: Return True if given parameter listname is an available name in the email list
        engine. Return False if listname already exists as a mailing list.
        """
        # VIESTIM: Is there a better return type than bool?
        raise NotImplementedError

    def add_email(self, email: str) -> None:
        """Add an email to email list"""
        raise NotImplementedError

    def delete_email(self, email: str) -> None:
        raise NotImplementedError

    def set_archive_type(self, archive: bool) -> None:
        raise NotImplementedError
