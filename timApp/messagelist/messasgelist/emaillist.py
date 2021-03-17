from dataclasses import dataclass

from flask import Response

from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')


@dataclass
class ListOptions:
    """All options regarding message lists."""
    listname: str
    domain: str
    archive: bool
    archiveType: str


@messagelist.route('/createlist', methods=['POST'])
def create_list(options: ListOptions) -> Response:
    """Handles creating a new message list.

    :param options All options regarding establishing a new message list.
    :return: A Response how the operation succeeded.
    """

    # create_new_email_list(listname, archive, domain)
    create_new_email_list(options)
    return ok_response()


# def create_new_email_list(listname: str, archive: bool, domain: str) -> None:
def create_new_email_list(options: ListOptions) -> None:
    """Creates a new mailing list.

    TODO: Complete
    This function is a stub, intended to complete when access to
    a safe testing environment with Mailman can be arranged.


    :param options All options regarding message lists.
    """

    print("Poor man's listname check:")
    print(options.listname + options.domain)
    print("archive? " + str(options.archive))
    print("archiveType? " + str(options.archiveType))
