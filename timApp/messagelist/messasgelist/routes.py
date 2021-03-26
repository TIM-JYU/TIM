from dataclasses import dataclass
from typing import List

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


@messagelist.route("/checkname/<string:name_candidate>", methods=['GET'])
def check_name(name_candidate: str) -> Response:
    """Check if parameter is unique in the pool of email list names.

    :param name_candidate: Possible name for message/email list.
    :return: Return a positive response if this name is
    unique and can be assigned to a message/email list. Return a negative response if name is not unique and
    therefore is already reserved for someone or something else.
    """
    exists = EmailListManager.check_name_availability(name_candidate)
    return json_response(exists)


@messagelist.route("/domains", methods=['GET'])
def domains() -> Response:
    """ Send possible domains for a client, if such exists.


    :return: If domains exists, return them as an array. If there are no domains, return an empty array.
    """
    # TODO: rewrite to check from a proper source. Now we just send this test data.
    possible_domains: List[str] = EmailListManager.get_domains()

    return json_response(possible_domains)
