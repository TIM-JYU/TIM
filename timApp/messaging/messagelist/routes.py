from dataclasses import dataclass
from typing import List

from flask import Response
from sqlalchemy.orm.exc import NoResultFound  # type: ignore

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_object
from timApp.messaging.messagelist.emaillist import EmailListManager, get_list_ui_link, create_new_email_list, \
    delete_email_list, check_emaillist_name_requirements, get_email_list_member, set_email_list_member_send_status, \
    set_email_list_member_delivery_status
from timApp.messaging.messagelist.emaillist import get_email_list_by_name, add_email
from timApp.messaging.messagelist.listoptions import ListOptions, ArchiveType, ReplyToListChanges
from timApp.messaging.messagelist.messagelist_models import MessageListModel, Channel, MessageListTimMember
from timApp.messaging.messagelist.messagelist_utils import check_messagelist_name_requirements, MessageTIMversalis, \
    new_list, archive_message, EmailAndDisplayName
from timApp.timdb.sqa import db
from timApp.user.groups import verify_groupadmin
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_info

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')

# TODO: These are for testing to get client side UI in sync with server side. When db is updated, these are then no
#  longer needed.
save_default_send_right: bool = True
save_default_delivery_right: bool = True
save_subject_prefix = ""
save_tim_user_can_join = False
save_only_text = False
save_default_reply_type = ReplyToListChanges.NOCHANGES


@messagelist.route('/createlist', methods=['POST'])
def create_list(options: ListOptions) -> Response:
    """Handles creating a new message list.

    :param options All options regarding establishing a new message list.
    :return: A Response with the list's management doc included. This way the creator can re-directed to the list's
    management page directly.
    """
    # VIESTIM: We assume here that email list will be created alongside message list. This might not be the case.
    verify_logged_in()
    verify_groupadmin()  # Creator of a list has to be a group admin.

    # Current user is set as the default owner.
    owner = get_current_user_object()

    options.name = options.name.strip()

    test_name(options.name)  # Test the name we are creating.

    manage_doc = new_list(options)
    create_new_email_list(options, owner)

    return json_response(manage_doc)


def test_name(name_candidate: str) -> None:
    normalized_name = name_candidate.strip()
    name, sep, domain = normalized_name.partition("@")
    check_messagelist_name_requirements(name)
    if sep:
        # If character '@' is found, we check email list specific name requirements.
        check_emaillist_name_requirements(name, domain)
    return


@messagelist.route("/checkname/<string:name_candidate>", methods=['GET'])
def check_name(name_candidate: str) -> Response:
    """Check if name candidate meets requirements.

    If name checking fails at any point, an exception is raised and that exception is delivered to the client. If all
    checks succeed, then just return an OK response.

    :param name_candidate: Possible name for message/email list. Should either be a name for a list or a fully qualifed
    domain name for (email) list. In the latter case we also check email list specific name requirements.
    """
    test_name(name_candidate)
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

    msg_list = MessageListModel.get_list_by_manage_doc_id(document_id)
    list_options = ListOptions(
        listname=msg_list.name,
        notifyOwnerOnListChange=msg_list.notify_owner_on_change,
        # VIESTIM: We need a better way of either querying or inferring list's (possible) domain. For the time being,
        #  here is a placeholder.
        domain="tim.jyu.fi",
        archive=msg_list.archive,
        # htmlAllowed=True,
        defaultReplyType=ReplyToListChanges.NOCHANGES,
        # TODO: Change to get these from db.
        timUsersCanJoin=save_tim_user_can_join,
        listSubjectPrefix=save_subject_prefix,
        canUnsubscribe=msg_list.can_unsubscribe,
        defaultSendRight=save_default_send_right,
        defaultDeliveryRight=save_default_delivery_right
    )
    if msg_list.email_list_domain:
        list_options.email_admin_url = get_list_ui_link(msg_list.name, msg_list.email_list_domain)
    if msg_list.info:
        list_options.list_info = msg_list.info
    if msg_list.description:
        list_options.list_description = msg_list.description
    return json_response(list_options)


@messagelist.route("/save", methods=['POST'])
def save_list_options(options: ListOptions) -> Response:
    verify_logged_in()
    # TODO: Additional checks for who get's to call this route.
    #  list's owner
    global save_tim_user_can_join
    global save_default_send_right
    global save_default_delivery_right
    global save_subject_prefix
    global save_tim_user_can_join
    global save_only_text
    global save_default_reply_type

    message_list = MessageListModel.get_list_by_name_exactly_one(options.name)

    # TODO: Verify that user has rights to the message list.

    if message_list.archive_policy != options.archive:
        # TODO: If message list changes it's archive policy, the members on the list need to be notified.
        message_list.archive = options.archive
        pass

    message_list.description = options.list_description
    message_list.info = options.list_info

    message_list.notify_owner_on_change = options.notify_owners_on_list_change

    # TODO: save the following list options.
    # message_list.can_unsubscribe
    # message_list.default_send_right
    # message_list.default_delivery_right

    # TODO: remove when these can be put onto db.
    log_info(f"***** ***** ***** *****")
    save_tim_user_can_join = options.tim_users_can_join
    log_info(f"tim user can join: {save_tim_user_can_join}")

    save_default_send_right = options.default_send_right
    log_info(f"default send right: {save_default_send_right}")

    save_default_delivery_right = options.default_delivery_right
    log_info(f"default delivery right: {save_default_delivery_right}")

    save_subject_prefix = options.list_subject_prefix
    log_info(f"subject prefix: {save_subject_prefix}")

    save_only_text = options.only_text
    log_info(f"only text: {save_only_text}")

    save_default_reply_type = options.default_reply_type
    log_info(f"default reply type: {save_default_reply_type}")
    log_info(f"***** ***** ***** *****")

    db.session.commit()
    return ok_response()


@dataclass
class MemberInfo:
    """Wrapper for information about a member on a message list."""
    name: str
    sendRight: bool
    deliveryRight: bool
    email: str


@messagelist.route("/savemembers", methods=['POST'])
def save_members(listname: str, members: List[MemberInfo]) -> Response:
    """Save the state of existing list members, e.g. send and delivery rights."""
    message_list = MessageListModel.get_list_by_name_exactly_one(listname)
    email_list = None
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)

    # VIESTIM: This solution is probably not well optimized.
    for member in members:
        db_member = message_list.get_member_by_name(member.name, member.email)
        # VIESTIM: In what case would we face a situation where we couldn't find this member? They are given from the
        #  db in the first place.
        if db_member:
            # If send or delivery right has changed, then set them to db and on Mailman.
            if db_member.send_right != member.sendRight:
                db_member.send_right = member.sendRight
                if email_list:
                    mlist_member = get_email_list_member(email_list, member.email)
                    set_email_list_member_send_status(mlist_member, member.deliveryRight)
            if db_member.delivery_right != member.deliveryRight:
                db_member.delivery_right = member.deliveryRight
                if email_list:
                    mlist_member = get_email_list_member(email_list, member.email)
                    set_email_list_member_delivery_status(mlist_member, member.deliveryRight, by_moderator=True)

            db.session.flush()

    db.session.commit()
    return ok_response()


@messagelist.route("/addmember", methods=['POST'])
def add_member(memberCandidates: List[str], msgList: str) -> Response:
    from timApp.user.user import User  # Local import to avoid cyclical imports.

    # TODO: Validate access rights.
    #  List owner.
    verify_logged_in()

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


@messagelist.route("/getmembers/<list_name>", methods=['GET'])
def get_members(list_name: str) -> Response:
    """Get members belonging to a certain list.

    :param list_name:
    :return:
    """
    verify_logged_in()
    # TODO: Verify user is a owner of the list.

    msg_list = MessageListModel.get_list_by_name_exactly_one(list_name)
    list_members = msg_list.get_individual_members()
    return json_response(list_members)


@messagelist.route("/archive", methods=['POST'])
def archive(message: MessageTIMversalis) -> Response:
    """Archive a message sent to a message list.

    :param message: The message to be archived.
    :return: Return OK response if everything went smoothly.
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
    """A testing route."""
    # VIESTIM: This fails if the message list doesn't exist.
    msg_list = MessageListModel.get_list_by_name_exactly_one("uusilista293u0")
    message = MessageTIMversalis(message_list_name="uusilista293u0",
                                 message_channel=Channel.EMAIL_LIST,
                                 sender=EmailAndDisplayName(email_address="tomi.t.lundberg@student.jyu.fi",
                                                            display_name="Tomi L."),
                                 recipients=[EmailAndDisplayName(email_address="status-check2@tim.jyu.fi",
                                                                 display_name="Uusilista293u0")],
                                 title="Viestin otsikko",
                                 message_body="Hei mualima!"
                                 )
    archive_message(message_list=msg_list, message=message)
    return ok_response()
