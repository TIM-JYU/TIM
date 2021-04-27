from dataclasses import dataclass, field
from typing import List, Optional

from flask import Response
from sqlalchemy.orm.exc import NoResultFound  # type: ignore

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import create_document
from timApp.document.docinfo import DocInfo
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.messaging.messagelist.emaillist import EmailListManager, create_new_email_list, delete_email_list
from timApp.messaging.messagelist.emaillist import get_email_list_by_name, add_email
from timApp.messaging.messagelist.listoptions import ListOptions, ArchiveType, ReplyToListChanges
from timApp.messaging.messagelist.messagelist_models import MessageListModel, Channel
from timApp.messaging.messagelist.messagelist_models import MessageListTimMember, get_members_for_list
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
    # VIESTIM: We assume here that email list will be created alongside message list. This might not be the case.
    verify_logged_in()
    # Current user is set as the default owner.
    owner = get_current_user_object()

    options.listname = options.listname.strip()

    manage_doc = new_list(options)
    create_new_email_list(options, owner)

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
    normalized_name = name_candidate.strip()
    name, sep, domain = normalized_name.partition("@")
    msg_list_exists = MessageListModel.name_exists(name)
    if msg_list_exists:
        raise RouteException(f"Message list with name {name} already exists.")
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
def delete_list(listname: str, domain: str) -> Response:
    """Delete message/email list. List name is provided in the request body.

    :param domain: If an empty string, message list is not considered to have a domain associated and therefore doesn't
     have an email list. If this is an unempty string, then an email list is excpected to also exist.
    :param listname: The list to be deleted. If the name does not contain '@', just delete  a message list. If it
     contains '@', we delete a message list and the corresponding email list.
    :return: A string describing how the operation went.
    """
    verify_logged_in()
    # TODO: Verify that the deleter is an owner of the message list.
    msg_list = MessageListModel.get_list_by_name_exactly_one(listname)
    # list_domain = msg_list.email_list_domain
    # TODO: Put message list deletion here.
    if domain:
        # A domain is given, so we are also looking to delete an email list.
        # VIESTIM: Perform a soft deletion for now.
        delete_email_list(f"{listname}@{domain}")
    return ok_response()


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


message_list_doc_prefix = "/messagelists"
message_list_archive_prefix = "/archives"


def create_management_doc(msg_list_model: MessageListModel, list_options: ListOptions) -> DocInfo:
    # TODO: Document should reside in owner's personal path.

    # VIESTIM: The management document is created on the message list creator's personal folder. This might be a good
    #  default, but if the owner is someone else than the creator then we have to handle that.

    # VIESTIM: We'll err on the side of caution and make sure the path is safe for the management doc.
    path_safe_list_name = remove_path_special_chars(list_options.listname)
    path_to_doc = f'/{message_list_doc_prefix}/{path_safe_list_name}'

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


@messagelist.route("/getlist/<int:document_id>", methods=['GET'])
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
        htmlAllowed=True,
        defaultReplyType=ReplyToListChanges.NOCHANGES
    )
    return json_response(list_options)


@messagelist.route("/addmember", methods=['POST'])
def add_member(memberCandidates: List[str], msgList: str) -> Response:
    from timApp.user.user import User  # Local import to avoid cyclical imports.

    try:
        msg_list = MessageListModel.get_list_by_name_exactly_one(msgList)
    except NoResultFound:
        raise RouteException(f"There is no list named {msgList}")

    # TODO: Implement checking whether or not users are just added to a list (like they are now) or they are invited
    #  to a list (requires link generation and other things).

    # TODO: Check if there is an email list attached to the message list.
    em_list = None
    if msg_list.email_list_domain is not None:
        em_list = get_email_list_by_name(msg_list.name, msg_list.email_list_domain)

    for member_candidate in memberCandidates:
        u = User.get_by_name(member_candidate.strip())
        if u is not None:
            # The name given was an existing TIM user.
            new_tim_member = MessageListTimMember()
            new_tim_member.message_list_id = msg_list.id
            new_tim_member.group_id = u.get_personal_group().id
            # VIESTIM: For convenience sake just add these. Figure out list rights at a later date. Everyone loves a
            #  bit of technical debt, don't they?
            new_tim_member.delivery_right = True
            new_tim_member.send_right = True
            db.session.add(new_tim_member)

            # VIESTIM: Get user's email and add it to list's email list.
            if em_list is not None:
                user_email = u.email  # TODO: Search possible additional emails.
                # TODO: Needs pre confirmation check from whoever adds members to a list on the client side. Now a
                #  placeholder value of True.
                add_email(em_list, user_email, email_owner_pre_confirmation=True, real_name=u.real_name,
                          send_right=new_tim_member.send_right, delivery_right=new_tim_member.delivery_right)

        # TODO: If member_candidate is a user group, what do? Add as is or open it to individual users?

        # TODO: If member candidate is not a user, or a user group, then we assume an external member. Add external
        #  members.

    db.session.commit()

    return ok_response()


@dataclass
class MemberInfo:
    """Wrapper for information about a member on a message list."""
    name: str
    sendRight: bool
    deliveryRight: bool
    email: str


@messagelist.route("/getmembers/<list_name>", methods=['GET'])
def get_members(list_name: str) -> Response:
    """Get members belonging to a certain list.

    :param list_name:
    :return:
    """
    from timApp.user.usergroup import UserGroup

    msg_list = MessageListModel.get_list_by_name_exactly_one(list_name)
    members = get_members_for_list(msg_list)
    list_members: List[MemberInfo] = []
    for member in members:
        if member.tim_member:
            gid = member.tim_member.group_id
            # VIESTIM: This should be the user's personal user group.
            ug = UserGroup.query.filter_by(id=gid).one()
            u = ug.users[0]
            mi = MemberInfo(name=u.real_name, email=u.email, sendRight=member.send_right,
                            deliveryRight=member.delivery_right)
        else:
            mi = MemberInfo(name="External member", email=member.external_member.email_address,
                            sendRight=member.send_right, deliveryRight=member.delivery_right)
        list_members.append(mi)

    return json_response(list_members)


@dataclass
class Message:
    # Meta information about where this message belongs to.
    message_list_name: str
    domain: Optional[str]
    message_channel: Channel = field(metadata={'by_value': True})

    # Header information
    sender: str
    reply_to: Optional[str]
    recipients: List[str]
    title: str

    # Message body
    message_body: str


@messagelist.route("/archive", methods=['POST'])
def archive(message: Message) -> Response:
    """Archive a message sent to a message list.

    :param message: The message to be archived.
    :return: Return OK response if everything went smoothly.
    """
    # VIESTIM: This view function has not been tested yet.

    msg_list = MessageListModel.get_list_by_name_first(message.message_list_name)

    if msg_list is None:
        raise RouteException(f"No message list with name {message.message_list_name} exists.")

    # TODO: Check rights to message list?

    archive_policy = msg_list.archive_policy

    # TODO: Check if this message list is archived at all in the first place, or if the message has had some special
    #  value that blocks archiving. Think X-No-Archive header on emails.
    if archive_policy is ArchiveType.NONE:
        raise RouteException("This list doesn't archive messages.")

    # TODO: If there are multiple messages with same title, differentiate them.
    archive_title = message.title
    archive_path = f"{message_list_archive_prefix}/{remove_path_special_chars(archive_title)}"

    # Archive folder for message list.
    archive_folder = Folder.find_by_location(archive_path, msg_list.name)

    archive_doc = create_document(archive_path, archive_title)

    # Set header information for archived message.
    archive_doc.document.add_text(f"Title: {message.title}")
    archive_doc.document.add_text(f"Sender: {message.sender}")
    archive_doc.document.add_text(f"Recipients: {message.recipients}")

    # Set message body for archived message.
    archive_doc.document.add_text(f"{message.message_body}")

    # From the archive folder, query all documents, sort them by created attribute. We do this to get the previously
    # newest archived message, before we create a archive document for newest message.
    all_archived_messages = []
    if archive_folder is not None:
        all_archived_messages = archive_folder.get_all_documents()
    else:
        # TODO: Set folder's owners to be message list's owners.
        manage_doc_block = Block.query.filter_by(id=msg_list.manage_doc_id).one()
        owners = manage_doc_block.owners()
        Folder.create(archive_path, owner_groups=owners, title=f"{msg_list.name}")

    if len(all_archived_messages) > 1:
        sorted_messages = sorted(all_archived_messages, key=lambda document: document.block.created, reverse=True)
        previous_doc = sorted_messages[1]

        # Set footer information for archived message. Footer information is not set for the very first message,
        # it get's it's link to next message when a second message is archived.

        # VIESTIM: Do we need other type of URL to previous_doc and archive_doc? Is url attribute enough?
        previous_doc_title = "Previous message"
        previous_doc_link = f"{previous_doc.url}"
        previous_message_link = f"[{previous_doc_title}]({previous_doc_link})"
        archive_doc.document.add_text(f"{previous_message_link}")

        next_doc_title = "Next message"
        next_doc_link = f"{archive_doc.url}"
        previous_doc.document.add_text(f"[{next_doc_title}]({next_doc_link})")

    # TODO: Set proper rights to the document. The message sender owns the document. Owners of the list get at least a
    #  view right. Other rights depend on the message list's archive policy.

    return ok_response()
