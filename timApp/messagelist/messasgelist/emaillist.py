from dataclasses import dataclass
from typing import List

from mailmanclient import Client

from timApp.tim_app import app

_client = None
"""A client object to utilize Mailmans REST API. Poke directly only when necessary, otherwise use via EmailListManager 
class."""

if app.config['MAILMAN_URL'] == "" or app.config['MAILMAN_USER'] == "" or app.config['MAILMAN_PASS']:
    print("Server started without configuration for Mailman connection. Is everthing the way it's supposed to be?")
else:
    _client = Client(app.config['MAILMAN_URL'], app.config['MAILMAN_USER'], app.config['MAILMAN_PASS'])


# VIESTIM Decorate class methods with @staticmethod unless the method would necessarily be needed for an instance of
#  the class. We wish to avoid instancing classes if possible.


@dataclass
class EmailListManager:
    """Functionality for chosen email list management system Mailman 3. Handels everything else except things
    spesific to existing email lists."""

    domains: List[str]
    """Possible domains which can be used with our instance of Mailman."""

    @staticmethod
    def check_name_availability(name_candidate: str) -> bool:
        """Search for a name from the pool of used email list names.

        :param name_candidate: The name to search for. The name needs to be a proper email list name,
        e.g. name@domain.org.
        :return: Return True if name is already in use. Return False if not
        """
        # TODO: Implement the search.
        return False

    @staticmethod
    def get_domains() -> List[str]:
        """

        :return: A list of possible domains.
        """
        # TODO: Change to return domains properly.
        possible_domains: List[str] = ["@lists.tim.jyu.fi", "@timlist.jyu.fi", "@lists.jyu.fi"]
        return possible_domains

    @staticmethod
    def _set_domains() -> None:
        """Set possible domains. Searches possible domains from a configure file."""
        # TODO: Search the proper configuration file(s) for domains.
        pass

    @staticmethod
    def create_new_list(name: str) -> None:
        """

        :param name: A full email list name, e.g. name@domain.org.
        :return:
        """
        print("testiprinti")
        print(name)


@dataclass
class EmailList:
    """Class to aid with email list spesific functionality attribute checking and changes.

    This class is designed to be used when an existing email list is expected to exits. Think operations like adding
    an email to an existing list etc. For operations other than mentioned, use EmailListManager.
    """

    # VIESTIM: Would it be polite to return something as an indication how the operation went?

    @staticmethod
    def set_archive_type(listname: str, archive: bool) -> None:
        pass

    @staticmethod
    def delete_email(listname: str, email: str) -> None:
        pass

    @staticmethod
    def add_email(listname: str, email: str) -> None:
        pass
