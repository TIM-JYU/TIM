from dataclasses import dataclass
from typing import List, Optional

from flask import Response

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import create_document
from timApp.document.docinfo import DocInfo
from timApp.messaging.messagelist.emaillist import EmailListManager, EmailList
from timApp.messaging.messagelist.listoptions import ListOptions, ArchiveType
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import remove_path_special_chars

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')


@messagelist.route('/createlist', methods=['POST'])
def create_list(options: ListOptions) -> Response:
    """Handles creating a new message list.

    :param options All options regarding establishing a new message list.
    :return: A Response with the list's management doc included. This way the creator can re-directed to the list's
    management page directly.
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

    If name checking fails at any point, an exception is raised and that exception is delivered to the client. If all
    checks succeed, then just return an OK response.

    :param name_candidate: Possible name for message/email list. Should either be a name for a list or a fully qualifed
    domain name for (email) list. In the latter case we also check email list specific name requirements.
    """

    name, sep, domain = name_candidate.partition("@")

    if sep:
        # If character '@' is found, we check email list specific name requirements.
        EmailListManager.check_name_requirements(name, domain)

    return ok_response()


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

    doc_info = create_management_doc(msg_list, list_options)

    db.session.commit()
    return doc_info


def create_management_doc(msg_list_model: MessageListModel, list_options: ListOptions) -> DocInfo:
    # TODO: Document should reside in owner's personal path.

    # VIESTIM: The management document is created on the message list creator's personal folder. This might be a good
    #  default, but if the owner is someone else than the creator then we have to handle that.
    creator = get_current_user_object()

    personal_path = creator.get_personal_folder().path

    # VIESTIM: We'll err on the side of caution and make sure the path is safe for the management doc.
    path_safe_list_name = remove_path_special_chars(list_options.listname)
    path_to_doc = f'/{personal_path}/{path_safe_list_name}'

    doc = create_document(path_to_doc, list_options.listname)

    # VIESTIM: We add the admin component to the document. This might have to be changed if the component is turned
    #  into a plugin.

    admin_component = """#- {allowangular="true"}
<tim-message-list-admin></tim-message-list-admin>
    """
    doc.document.add_text(admin_component)

    # Set the management doc for the message list.
    msg_list_model.manage_doc_id = doc.id

    return doc


@messagelist.route("/getlist/<document_id>", methods=['GET'])
def get_list(document_id: int) -> Response:
    """Get the information for a message list.

    :param document_id: ID for message list's admin document.
    :return: ListOptions with the list's information.
    """
    msg_list = MessageListModel.get_list_by_manage_doc_id(document_id)
    list_options = ListOptions(
        listname=msg_list.name,
        listInfo=msg_list.info,
        listDescription=msg_list.description,
        notifyOwnerOnListChange=msg_list.notify_owner_on_change,
        # VIESTIM: We need a better way of either querying or inferring list's (possible) domain. For the time being,
        #  here is a placeholder.
        domain="tim.jyu.fi",
        archive=msg_list.archive,
        # TODO: Query members.
        emails=[],
        # TODO: Replace placeholder once we can properly query the owners email.
        ownerEmail="totalund@student.jyu.fi"
    )
    return json_response(list_options)


@messagelist.route("/archive/<message_list_name>", methods=['POST'])
def archive(message_list_name: str, message: str) -> Response:
    """Archive a message sent to a message list.

    :param message_list_name: Which message list is the message intented to.
    :param message: The message to be archived.
    :return: Return OK response if everything went smoothly.
    """
    # VIESTIM: This view function has not been tested yet. It's implementation is still badly a work in progress.

    msg_list = MessageListModel.get_list_by_name(message_list_name)
    if msg_list is None:
        raise RouteException(f"No message list with name {message_list_name} exists.")

    # TODO: Check rights to message list.
    # TODO: Get the message list's archive policy.

    archive_policy = msg_list.archive_policy()
    # TODO: Check if this message list is archived at all in the first place, or if the message has had some special
    if archive_policy is ArchiveType.NONE:
        raise RouteException("This list doesn't archive messages.")

    #  value that blocks archiving. Think X-No-Archive header on emails.
    # TODO: Create document.
    archive_path = "/archives/"
    archive_title = "This is a placeholder title"
    archive_doc = create_document(archive_path, archive_title)

    # TODO: Add header information for archived message. Now just a placeholder text.
    archive_doc.document.add_text("Header information")

    # TODO: Add the message body. Now just a placeholder text.
    archive_doc.document.add_text("Message body.")

    # TODO: Add footer information. Now just a placeholder text.
    archive_doc.document.add_text("Footer information.")

    # TODO: Get the newest document in the archive folder (before this archived message). Would this be easier if we
    #  query for this before we create the new document?
    # TODO: Update the previous latest archived message's footer to point to the newest message.

    # TODO: Set proper rights to the document. The message sender owns the document. Owner of the list gets at least a
    #  view right. Other rights depend on the message list's archive policy.

    return ok_response()
