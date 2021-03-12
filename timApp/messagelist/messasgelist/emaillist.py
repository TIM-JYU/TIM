from flask import Response, request

from timApp.util.flask.responsehelper import ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')


@messagelist.route('/createlist', methods=['POST'])
def create_list() -> Response:
    """Handles creating a new message list.

    This function assumes that the request proxy's request body is a JSON
    object with certain specified fields, see new-message-list.component.ts

    Flask's Request is used to carry over the data.

    :return: A Response how the operation succeeded.
    """

    # TODO: Explore the possibility of a more sane datatype to use than
    # direct object/dict type.
    json_object: dict = request.get_json()

    create_new_email_list(json_object)

    return ok_response()


def create_new_email_list(obj: dict):
    """Creates a new mailing list.

    TODO: Complete
    This function is a stub, intended to complete when access to
    a safe testing environment with Mailman can be arranged.

    :param obj: A Python object structure
    """

    listname: str = obj["listname"]
    domain: str = obj["domain"]
    print("Poor man's listname check:")
    print(listname + domain)
