from dataclasses import dataclass
from typing import List, Optional

from flask import Response

from timApp.messagelist.messasgelist.emaillist import EmailListManager
from timApp.util.flask.responsehelper import ok_response, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')


@dataclass
class ListOptions:
    """All options regarding message lists."""
    listname: str
    domain: str
    archive: bool
    archiveType: str
    emails: List[str]


@messagelist.route('/createlist', methods=['POST'])
def create_list(options: ListOptions) -> Response:
    """Handles creating a new message list.

    :param options All options regarding establishing a new message list.
    :return: A Response how the operation succeeded.
    """

    create_new_email_list(options)
    return ok_response()


def create_new_email_list(options: ListOptions) -> None:
    """Creates a new mailing list.

    TODO: Complete, this function is a stub, intended to complete when access to a safe testing environment with
     Mailman can be arranged.


    :param options All options regarding message lists.
    """

    print("Poor man's listname check:")
    EmailListManager.create_new_list(options.listname + options.domain)
    print("archive? " + str(options.archive))
    print("archiveType? " + str(options.archiveType))
    print("emails: " + str(options.emails))


@dataclass
class NameCheckInfo:
    """Return information about name check results."""
    nameOK: Optional[bool] = None
    explanation: str = ""


@messagelist.route("/checkname/<string:name_candidate>", methods=['GET'])
def check_name(name_candidate: str) -> Response:
    """Check if name candidate meets requirements.

    :param name_candidate: Possible name for message/email list. Should either be a name for a list or a fully qualifed
    domain name for (email) list. In the latter case we also check email list specific name requirements.
    :return: Return a response of form class NameCheckInfo. The value nameOk is the "in the nutshell"
    explanation how the check went. If connection to Mailman failed, then return None. If name is both available and
    conforms to naming rules, return True. Otherwise return False. In all cases, also return an explanatory string as
    the explanation value.
    """

    name, sep, domain = name_candidate.partition("@")

    # TODO: add message list name requirement check. Now just use dummy value to get going.
    messagelist_requirements: bool = True
    messagelist_explanation: str = ""

    email_requirements: Optional[bool] = False
    email_explanation: str = ""

    if sep:
        # If character '@' is found, we check email list spesific name requirements.
        email_requirements, email_explanation = EmailListManager.check_name_requirements(name, domain)

    # Initialize a response.
    response = NameCheckInfo()

    # Go through possibilities of name check outcomes.
    if email_requirements is None:
        # Connection to Mailman or it's host server failed.
        response.explanation = email_explanation
    elif email_requirements and messagelist_requirements:
        response.nameOK = True
        response.explanation = "Name is available and it meets requirements."
    elif not messagelist_requirements:
        response.nameOk = False
        response.explanation = messagelist_explanation
    elif not email_requirements:
        response.nameOk = False
        response.explanation = email_explanation
        pass

    return json_response(response)


@messagelist.route("/domains", methods=['GET'])
def domains() -> Response:
    """ Send possible domains for a client, if such exists.

    :return: If domains exists, return them as an array. If there are no domains, return an empty array.
    """
    possible_domains: List[str] = EmailListManager.get_domain_names()

    return json_response(possible_domains)
