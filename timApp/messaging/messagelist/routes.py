from dataclasses import dataclass
from datetime import datetime
from typing import List, Optional

from flask import Response

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_object
from timApp.messaging.messagelist.emaillist import get_email_list_by_name
from timApp.messaging.messagelist.emaillist import get_list_ui_link, create_new_email_list, \
    delete_email_list, check_emaillist_name_requirements, get_domain_names, verify_mailman_connection
from timApp.messaging.messagelist.listoptions import ListOptions, ArchiveType, Distribution
from timApp.messaging.messagelist.messagelist_models import MessageListModel, Channel
from timApp.messaging.messagelist.messagelist_utils import check_messagelist_name_requirements, MessageTIMversalis, \
    new_list, archive_message, EmailAndDisplayName, set_message_list_notify_owner_on_change, \
    set_message_list_member_can_unsubscribe, set_message_list_subject_prefix, set_message_list_tim_users_can_join, \
    set_message_list_default_send_right, set_message_list_default_delivery_right, set_message_list_only_text, \
    set_message_list_non_member_message_pass, set_message_list_allow_attachments, set_message_list_default_reply_type, \
    add_new_message_list_tim_user, add_new_message_list_group, add_message_list_external_email_member, \
    set_message_list_member_removed_status, set_member_send_delivery, set_message_list_description, \
    set_message_list_info
from timApp.timdb.sqa import db
from timApp.user.groups import verify_groupadmin
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException, is_localhost
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import is_valid_email, get_current_time

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')


@messagelist.route('/createlist', methods=['POST'])
def create_list(options: ListOptions) -> Response:
    """Handles creating a new message list.

    :param options All options regarding establishing a new message list.
    :return: A Response with the list's management doc included. This way the creator can re-directed to the list's
    management page directly.
    """
    verify_logged_in()
    verify_groupadmin()  # Creator of a list has to be a group admin.

    # Until other message channels make email list's optional, it is required that connection to Mailman is
    # configured when creating message lists.
    verify_mailman_connection()

    # Current user is set as the default owner.
    owner = get_current_user_object()

    options.name = options.name.strip()

    test_name(options.name)  # Test the name we are creating.

    manage_doc, message_list = new_list(options)

    if options.domain:
        create_new_email_list(options, owner)
        # Add the domain to a message list only after the email list has been created. This way if the list creation
        # fails, we have indication that the list does not have an email list attached to it.
        message_list.email_list_domain = options.domain

    db.session.commit()
    return json_response(manage_doc)


def test_name(name_candidate: str) -> None:
    """Check new message list's name candidate's name.

     The name has to meet naming rules, it has to be not already be in use and it cannot be a reserved name. If the
     function retuns control to it's caller, then name is viable to use for a message list. If at some point the name
     is not viable, then an exception is raised.

    :param name_candidate: The name candidate to check.
    """
    normalized_name = name_candidate.strip()
    name, sep, domain = normalized_name.partition("@")
    check_messagelist_name_requirements(name)
    if sep:
        # If character '@' is found, we check email list specific name requirements.
        verify_mailman_connection()
        check_emaillist_name_requirements(name, domain)


@messagelist.route("/checkname/<string:name_candidate>", methods=['GET'])
def check_name(name_candidate: str) -> Response:
    """Check if name candidate meets requirements.

    If name checking fails at any point, an exception is raised and that exception is delivered to the client. If all
    checks succeed, then just return an OK response.

    :param name_candidate: Possible name for message/email list. Should either be a name for a list or a fully qualifed
    domain name for (email) list. In the latter case we also check email list specific name requirements.
    :return: OK response.
    """
    test_name(name_candidate)
    return ok_response()


@messagelist.route("/domains", methods=['GET'])
def domains() -> Response:
    """Send possible domains for a client, if such exists.

    :return: If domains are configured, return them as an array.
    """
    verify_mailman_connection()
    possible_domains: List[str] = get_domain_names()
    return json_response(possible_domains)


@messagelist.route("/deletelist", methods=['DELETE'])
def delete_list(listname: str, domain: str) -> Response:
    """Delete message/email list. List name is provided in the request body.

    :param domain: If an empty string, message list is not considered to have a domain associated and therefore doesn't
     have an email list. If this is an nonempty string, then an email list is excpected to also exist.
    :param listname: The list to be deleted.
    :return: A string describing how the operation went.
    """
    verify_logged_in()
    # TODO: Additional checks for who get's to call this route.
    #  Deleter has to be an owner of the list.

    # TODO: Verify that the deleter is an owner of the message list.
    msg_list = MessageListModel.get_list_by_name_exactly_one(listname)
    # list_domain = msg_list.email_list_domain
    # TODO: Put message list deletion here.
    if domain:
        # A domain is given, so we are also looking to delete an email list.
        # VIESTIM: Perform a soft deletion for now.
        delete_email_list(f"{listname}@{domain}")
    return ok_response()


@messagelist.route("/getlist/<int:document_id>", methods=['GET'])
def get_list(document_id: int) -> Response:
    """Get the information for a message list.

    :param document_id: ID for message list's admin document.
    :return: ListOptions with the list's information.
    """
    verify_logged_in()
    # TODO: Additional checks for who gets to call this route.

    message_list = MessageListModel.get_list_by_manage_doc_id(document_id)
    list_options = ListOptions(
        name=message_list.name,
        notify_owners_on_list_change=message_list.notify_owner_on_change,
        domain=message_list.email_list_domain,
        archive=message_list.archive,
        default_reply_type=message_list.default_reply_type,
        tim_users_can_join=message_list.tim_user_can_join,
        list_subject_prefix=message_list.subject_prefix,
        members_can_unsubscribe=message_list.can_unsubscribe,
        default_send_right=message_list.default_send_right,
        default_delivery_right=message_list.default_delivery_right,
        only_text=message_list.only_text,
        non_member_message_pass=message_list.non_member_message_pass,
        email_admin_url=get_list_ui_link(message_list.name, message_list.email_list_domain),
        list_info=message_list.info,
        list_description=message_list.description,
        allow_attachments=message_list.allow_attachments,
        distribution=Distribution(email_list=True, tim_message=True),
    )
    return json_response(list_options)


@messagelist.route("/save", methods=['POST'])
def save_list_options(options: ListOptions) -> Response:
    """Save message list's options.

    :param options: The options to be saved.
    :return: OK response.
    """
    verify_logged_in()
    # TODO: Additional checks for who get's to call this route.
    #  list's owner

    message_list = MessageListModel.get_list_by_name_exactly_one(options.name)

    # TODO: Verify that user has rights to the message list.

    if message_list.archive_policy != options.archive:
        # TODO: If message list changes it's archive policy, the members on the list need to be notified.
        message_list.archive = options.archive

    set_message_list_description(message_list, options.list_description)
    set_message_list_info(message_list, options.list_info)
    set_message_list_notify_owner_on_change(message_list, options.notify_owners_on_list_change)
    set_message_list_member_can_unsubscribe(message_list, options.members_can_unsubscribe)
    set_message_list_subject_prefix(message_list, options.list_subject_prefix)
    set_message_list_only_text(message_list, options.only_text)
    set_message_list_allow_attachments(message_list, options.allow_attachments)
    set_message_list_tim_users_can_join(message_list, options.tim_users_can_join)
    set_message_list_non_member_message_pass(message_list, options.non_member_message_pass)

    # TODO: Implemented client side.
    set_message_list_default_send_right(message_list, options.default_send_right)
    # TODO: Implemented client side.
    set_message_list_default_delivery_right(message_list, options.default_delivery_right)

    # TODO: Implement client side.
    set_message_list_default_reply_type(message_list, options.default_reply_type)
    # TODO: set the following list options.
    # TODO: Implement client side
    # message_list.distribution = options.distribution

    db.session.commit()
    return ok_response()


@dataclass
class MemberInfo:
    """Wrapper for information about a member on a message list."""
    name: str
    sendRight: bool
    deliveryRight: bool
    email: str
    removed: Optional[datetime] = None


@messagelist.route("/savemembers", methods=['POST'])
def save_members(listname: str, members: List[MemberInfo]) -> Response:
    """Save the state of existing list members, e.g. send and delivery rights.

    :param listname: The name of the message list where the members will be saved.
    :param members: The members to be saved.
    :return: Response for the client. The Response is a simple ok_response().
    """
    message_list = MessageListModel.get_list_by_name_exactly_one(listname)
    email_list = None
    if message_list.email_list_domain:
        verify_mailman_connection()
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)

    # VIESTIM: This solution is probably not well optimized.
    for member in members:
        db_member = message_list.get_member_by_name(member.name, member.email)
        # VIESTIM: In what case would we face a situation where we couldn't find this member? They are given from the
        #  db in the first place.
        if db_member:
            # If send or delivery right has changed, then set them to db and on Mailman.
            set_member_send_delivery(db_member, member.sendRight, member.deliveryRight, email_list=email_list)
            set_message_list_member_removed_status(db_member, member.removed, email_list=email_list)
    db.session.commit()
    return ok_response()


@messagelist.route("/addmember", methods=['POST'])
def add_member(memberCandidates: List[str], msgList: str, sendRight: bool, deliveryRight: bool) -> Response:
    """Add new members to a message list.

    :param memberCandidates: Names of member candidates.
    :param msgList: The message list where we are trying to add new members.
    :param sendRight: The send right on a list for all the member candidates.
    :param deliveryRight: The delivery right on a list for all the member candidates.
    :return: OK response.
    """
    # TODO: Validate access rights.
    #  List owner.
    verify_logged_in()

    msg_list = MessageListModel.get_list_by_name_exactly_one(msgList)

    # TODO: Implement checking whether or not users are just added to a list (like they are now) or they are invited
    #  to a list (requires link generation and other things).

    em_list = None
    if msg_list.email_list_domain is not None:
        verify_mailman_connection()
        em_list = get_email_list_by_name(msg_list.name, msg_list.email_list_domain)

    for member_candidate in memberCandidates:
        u = User.get_by_name(member_candidate.strip())
        if u is not None:
            # The name given was an existing TIM user.
            add_new_message_list_tim_user(msg_list, u, sendRight, deliveryRight, em_list)

        # TODO: If member_candidate is a user group, what do? Add as is or open it to individual users?
        ug = UserGroup.get_by_name(member_candidate.strip())
        if ug is not None:
            # The name belongs to a user group.
            add_new_message_list_group(msg_list, ug, sendRight, deliveryRight, em_list)
        # If member candidate is not a user, or a user group, then we assume an external member. Add external members.
        if is_valid_email(member_candidate.strip()) and em_list:
            add_message_list_external_email_member(msg_list, member_candidate.strip(),
                                                   sendRight, deliveryRight, em_list, None)
    db.session.commit()
    return ok_response()


@messagelist.route("/getmembers/<list_name>", methods=['GET'])
def get_members(list_name: str) -> Response:
    """Get members belonging to a certain list.

    :param list_name: The list where we are querying all the members.
    :return: All the members of a list.
    """
    verify_logged_in()
    # TODO: Verify user is a owner of the list.

    msg_list = MessageListModel.get_list_by_name_exactly_one(list_name)
    list_members = msg_list.get_tim_members()
    return json_response(list_members)


@messagelist.route("/archive", methods=['POST'])
def archive(message: MessageTIMversalis) -> Response:
    """Archive a message sent to a message list.

    :param message: The message to be archived.
    :return: OK response
    """
    # VIESTIM: This view function might be unnecessary. Probably all different message channels have to use their own
    #  handling routes for parsing purposes, and then possible archiving happens there.

    msg_list = MessageListModel.get_list_by_name_first(message.message_list_name)
    if msg_list is None:
        raise RouteException(f"No message list with name {message.message_list_name} exists.")

    # TODO: Check rights to message list?

    # TODO: Check if this message list is archived at all in the first place, or if the message has had some special
    #  value that blocks archiving. Think X-No-Archive header on emails.
    archive_policy = msg_list.archive_policy
    if archive_policy is ArchiveType.NONE:
        raise RouteException("This list doesn't archive messages.")

    archive_message(msg_list, message)

    return ok_response()


@messagelist.route("/test", methods=['GET'])
def test_route() -> Response:
    """A testing route. Only allow calls here during development, i.e. when operating from localhost."""
    if not is_localhost():
        raise RouteException()

    msg_list = MessageListModel.get_list_by_name_exactly_one("yet_another_list3")
    message = MessageTIMversalis(message_list_name=msg_list.name,
                                 message_channel=Channel.EMAIL_LIST,
                                 sender=EmailAndDisplayName(email_address="tomi.t.lundberg@student.jyu.fi",
                                                            display_name="Tomi L."),
                                 recipients=[EmailAndDisplayName(email_address="yet_another_list3@tim.jyu.fi",
                                                                 display_name="Uusilista293u0")],
                                 subject=f"Viestin otsikko {get_current_time()}",
                                 message_body="Hei mualima!"
                                 )
    archive_message(message_list=msg_list, message=message)
    return ok_response()
