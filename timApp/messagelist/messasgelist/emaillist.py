from dataclasses import dataclass
from typing import List

from mailmanclient import Client, MailingList, Domain

from timApp.tim_app import app

_client = None
"""A client object to utilize Mailmans REST API. Poke directly only when necessary, otherwise use via EmailListManager 
class."""

if "MAILMAN_URL" not in app.config or "MAILMAN_PASS" not in app.config or "MAILMAN_PASS" not in app.config:
    # No configuration for Mailman found.
    pass
elif app.config['MAILMAN_URL'] == "" or app.config['MAILMAN_USER'] == "" or app.config['MAILMAN_PASS'] == "":
    # Only placeholder configuration for Mailman found.
    print("Server started without proper configuration for Mailman connection.")
else:
    # All Mailman confiration values exist and are something other than an empty strgin.
    _client = Client(app.config['MAILMAN_URL'], app.config['MAILMAN_USER'], app.config['MAILMAN_PASS'])
    # TODO: Test connection somehow?


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
        :return: Return True if name is available. Return False if not.
        """
        if _client is not None:
            mlists: List[MailingList] = _client.get_lists()
        else:
            # TODO: Replace bad return value, this doesn't inform the caller that we can't perform this operation.
            return False
        for name in [mlist.fqdn_listname for mlist in mlists]:
            if name_candidate == name:
                return False
        return True

    @staticmethod
    def get_domains() -> List[str]:
        """

        :return: A list of possible domains.
        """
        # TODO: Change to return domains properly.
        possible_domains: List[str] = ["@foo.bar", "@lists.tim.jyu.fi", "@timlist.jyu.fi", "@lists.jyu.fi", "@example"
                                                                                                            ".com"]
        return possible_domains

    @staticmethod
    def _set_domains() -> None:
        """Set possible domains. Searches possible domains from a configure file."""
        # TODO: Search the proper configuration file(s) for domains.
        pass

    @staticmethod
    def create_new_list(name: str) -> None:
        """Create a new email list.

        :param name: A full email list name, e.g. name@domain.org.
        :return:
        """
        full_list_name: List[str] = name.split("@")

        # TODO: check that name matches requirements
        if EmailListManager.check_name_availability(name):
            print("list " + name + " does not exist yet, creating...")
            # TODO: Retrieve domain instead of creating it here,
            #  i.e. domain: Domain = _client.get_domain(full_list_name[1]),
            #  however get_domain does not currently work for some reason.
            #  This temporary implementation can be run only once per domain name between restarts.
            domain: Domain = _client.create_domain(full_list_name[1])
            email_list: MailingList = domain.create_list(full_list_name[0])
        else:
            print("list " + name + " already exists")


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
