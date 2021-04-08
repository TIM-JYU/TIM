from dataclasses import dataclass
from typing import List, Optional

from flask import Response

from timApp.messaging.messagelist.emaillist import EmailListManager
from timApp.util.flask.responsehelper import ok_response, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

messagelist = TypedBlueprint('messaging', __name__, url_prefix='/messaging')


@messagelist.route('/createlist', methods=['POST'])
def create_list(options: ListOptions) -> Response:
    """Handles creating a new message list.

    :param options All options regarding establishing a new message list.
    :return: A Response how the operation succeeded.
    """

    EmailListManager.create_new_list(options)
    return ok_response()


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
        # If character '@' is found, we check email list specific name requirements.
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
        response.nameOK = False
        response.explanation = messagelist_explanation
    elif not email_requirements:
        response.nameOK = False
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


@messagelist.route("/deletelist", methods=['DELETE'])
def delete_list(listname: str) -> Response:
    """Delete message/email list. List name is provided in the request body.

    :param listname: The list to be deleted. If the name does not contain '@', just delete  a message list. If it
     contains '@', we delete a message list and the corresponding email list.
    :return: A string describing how the operation went.
    """
    # TODO: User authentication. We can't let just anyone delete a list just because they can type the name.
    list_name, sep, domain = listname.partition("@")
    r = ""
    if domain:
        # A domain is given, so we are also looking to delete an email list.
        # Notice parameter. We give the fqdn list name to delete_list(), not plain list_name.
        r = EmailList.delete_list(listname)
    # TODO: Put message list deletion here.
    return json_response(r)
