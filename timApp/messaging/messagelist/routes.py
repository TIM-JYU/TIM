from dataclasses import dataclass
from typing import List, Optional

from flask import Response

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import create_document
from timApp.document.docinfo import DocInfo
from timApp.messaging.messagelist.emaillist import EmailListManager, EmailList
from timApp.messaging.messagelist.listoptions import ListOptions
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.timdb.sqa import db
from timApp.util.flask.responsehelper import ok_response, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import remove_path_special_chars

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')


@messagelist.route('/createlist', methods=['POST'])
def create_list(options: ListOptions) -> Response:
    """Handles creating a new message list.

    :param options All options regarding establishing a new message list.
    :return: A Response how the operation succeeded.
    """
    manage_doc = new_list(options)
    EmailListManager.create_new_list(options)
    return json_response(manage_doc)


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


def new_list(list_options: ListOptions) -> DocInfo:
    """Adds a new message list into the database and creates the list's management doc.

    :param list_options: The list information for creating a new message list.
    :return: The management document.
    """
    # VIESTIM: Check creation permission? Or should it be in the calling view function?
    msg_list = MessageListModel(name=list_options.listname, archive=list_options.archive)
    db.session.add(msg_list)

    doc_info = MessageListModel.create_management_doc(list_options)

    db.session.commit()
    return doc_info


def create_management_doc(msg_list_model, list_options: ListOptions) -> DocInfo:
    # TODO: Document should reside in owner's personal path.

    creator = get_current_user_object()
    personal_path = creator.get_personal_folder()
    # VIESTIM: We'll err on the side of caution and make sure the path is safe for the management doc.
    path_safe_list_name = remove_path_special_chars(list_options.listname)

    doc = create_document(f'/{personal_path}/{path_safe_list_name}', list_options.listname)

    test_text = f"""Welcome to a new message list,{creator.name}"""
    doc.document.add_text(test_text)

    return doc
