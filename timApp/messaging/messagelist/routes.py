from dataclasses import dataclass
from typing import List, Optional

from flask import Response
from sqlalchemy.orm.exc import NoResultFound  # type: ignore

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import create_document
from timApp.document.docinfo import DocInfo
from timApp.messaging.messagelist.emaillist import EmailListManager, EmailList, get_email_list_by_name, add_email
from timApp.messaging.messagelist.listoptions import ListOptions
from timApp.messaging.messagelist.messagelist_models import MessageListModel, MessageListTimMember, get_members_for_list
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


@messagelist.route("/getlist/<int:document_id>", methods=['GET'])
def get_list(document_id: int) -> Response:
    """Get the information for a message list.

    :param document_id: ID for message list's admin document.
    :return: ListOptions with the list's information.
    """
    msg_list = MessageListModel.get_list_by_manage_doc_id(document_id)
    # TODO: Maybe don't use ListOptions since ownerEmail and notifyOwnerOnListChange is not used there
    list_options = ListOptions(
        listname=msg_list.name,
        listInfo=msg_list.info,
        listDescription=msg_list.description,
        notifyOwnerOnListChange=msg_list.notify_owner_on_change,
        # VIESTIM: We need a better way of either querying or inferring list's (possible) domain. For the time being,
        #  here is a placeholder.
        domain="tim.jyu.fi",
        archive=msg_list.archive,
        # TODO: Replace placeholder once we can properly query the owners email.
    )
    return json_response(list_options)


@messagelist.route("/addmember", methods=['POST'])
def add_member(memberCandidates: List[str], msgList: str) -> Response:
    from timApp.user.user import User  # Local import to avoid cyclical imports.

    try:
        msg_list = MessageListModel.get_list_by_name(msgList)
    except NoResultFound:
        raise RouteException(f"There is no list named {msgList}")

    # TODO: Implement checking whether or not users are just added to a list (like they are now) or they are invited
    #  to a list (requires link generation and other things).

    # TODO: Check if there is an email list attached to the message list.
    em_list = None
    if msg_list.email_list_domain is not None:
        em_list = get_email_list_by_name(msg_list.name, msg_list.email_list_domain)

    for member_candidate in memberCandidates:
        u = User.get_by_name(member_candidate)
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

    msg_list = MessageListModel.get_list_by_name(list_name)
    members = get_members_for_list(msg_list)
    list_members: List[MemberInfo] = []
    for member in members:
        if member.tim_member:
            gid = member.group_id
            # VIESTIM: This should be the user's personal user group.
            ug = UserGroup.query.filter_by(id=gid).one()
            u = ug.users[0]
            mi = MemberInfo(name=u.real_name,email=u.email, sendRight=member.send_right,
                            deliveryRight=member.delivery_right)
        else:
            mi = MemberInfo(name="External member", email=member.external_member.email_address,
                            sendRight=member.send_right, deliveryRight=member.delivery_right)
        list_members.append(mi)
        # list_members.append(d)

    return json_response(list_members)
