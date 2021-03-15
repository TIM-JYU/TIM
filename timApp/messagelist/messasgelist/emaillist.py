from flask import Response

from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')


@messagelist.route('/createlist', methods=['POST'])
def create_list(listname: str, archive: bool, domain: str) -> Response:
    """Handles creating a new message list.

    :param listname The name of the email-list
    :param domain Selected domain for the email-list
    :param archive Whether or not this email-list will be archived
    :return: A Response how the operation succeeded.
    """

    create_new_email_list(listname, domain, archive)

    return ok_response()


def create_new_email_list(listname: str, domain: str, archive: str) -> None:
    """Creates a new mailing list.

    TODO: Complete
    This function is a stub, intended to complete when access to
    a safe testing environment with Mailman can be arranged.

    :param listname The name of the email-list
    :param domain Selected domain for the email-list
    :param archive Whether or not this email-list will be archived
    """

    print("Poor man's listname check:")
    print(listname + domain)
    print("archive? " + str(archive))
