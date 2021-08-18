from typing import List, Optional, Dict, Any

from flask import Response
from sqlalchemy.orm import load_only

from timApp.auth.accesshelper import verify_logged_in, has_manage_access, get_doc_or_abort, verify_manage_access, \
    verify_view_access
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import move_document, DocInfo
from timApp.folder.folder import Folder
from timApp.item.manage import get_trash_folder
from timApp.messaging.messagelist.emaillist import create_new_email_list, \
    delete_email_list, verify_emaillist_name_requirements, get_domain_names, verify_mailman_connection
from timApp.messaging.messagelist.emaillist import get_email_list_by_name
from timApp.messaging.messagelist.listinfo import ListInfo, MemberInfo, GroupAndMembers
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.messaging.messagelist.messagelist_utils import verify_messagelist_name_requirements, new_list, \
    set_message_list_notify_owner_on_change, \
    set_message_list_member_can_unsubscribe, set_message_list_subject_prefix, set_message_list_tim_users_can_join, \
    set_message_list_default_send_right, set_message_list_default_delivery_right, set_message_list_only_text, \
    set_message_list_non_member_message_pass, set_message_list_allow_attachments, set_message_list_default_reply_type, \
    add_new_message_list_group, add_message_list_external_email_member, \
    set_message_list_member_removed_status, set_member_send_delivery, set_message_list_description, \
    set_message_list_info, check_name_rules, verify_can_create_lists
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_error
from timApp.util.utils import is_valid_email, get_current_time

messagelist = TypedBlueprint('messagelist', __name__, url_prefix='/messagelist')


@messagelist.route('/checkname', methods=['POST'])
def check_name(name: str) -> Response:
    verify_logged_in()
    verify_can_create_lists()

    rule_fails = check_name_rules(name)
    exists = MessageListModel.name_exists(name)
    return json_response({
        'rule_fails': list(rule_fails),
        'exists': exists
    })


@messagelist.post('/createlist')
def create_list(options: ListInfo) -> Response:
    """Handles creating a new message list.

    :param options All options necessary for establishing a new message list.
    :return: A Response with the list's management doc included. This way the creator can re-directed to the list's
    management page directly.
    """
    verify_logged_in()
    verify_can_create_lists()

    # Current user is set as the default owner.
    owner = get_current_user_object()

    # Options object is given directly to new_list, so we don't want to use temporary variable for stripped name.
    options.name = options.name.strip()

    test_name(options.name)
    manage_doc, message_list = new_list(options)

    if options.domain:
        verify_mailman_connection()
        create_new_email_list(options, owner)
        # Add the domain to a message list only after the email list has been created. This way if the list creation
        # fails, we have indication that the list does not have an email list attached to it.
        message_list.email_list_domain = options.domain

    set_message_list_subject_prefix(message_list, f"[{message_list.name}]")

    db.session.commit()
    return json_response(manage_doc)


def test_name(name_candidate: str) -> None:
    """Check new message list's name candidate's name.

     The name has to meet naming rules, it has to be not already be in use and it cannot be a reserved name. If the
     function retuns control to its caller, then name is viable to use for a message list. If at some point the name
     is not viable, then an exception is raised.

    :param name_candidate: The name candidate to check.
    """
    normalized_name = name_candidate.strip()
    name, sep, domain = normalized_name.partition("@")
    verify_messagelist_name_requirements(name)
    if sep:
        # If character '@' is found, we check email list specific name requirements.
        verify_mailman_connection()
        verify_emaillist_name_requirements(name, domain)


@messagelist.get("/domains")
def domains() -> Response:
    """Send possible domains for a client, if such exists.

    :return: If domains are configured, return them as an array.
    """
    verify_mailman_connection()
    possible_domains: List[str] = get_domain_names()
    return json_response(possible_domains)


@messagelist.delete("/deletelist")
def delete_list(listname: str, permanent: bool) -> Response:
    """Delete message and  associated message channels.

    :param listname: The list to be deleted.
    :param permanent: A boolean flag indicating if the deletion is meant to be permanent.
    :return: A string describing how the operation went.
    """
    # Check access rights.
    verify_logged_in()
    message_list = MessageListModel.from_name(listname)
    if not has_manage_access(message_list.block):
        raise RouteException("You need at least a manage access to the list in order to do this action.")

    # The amount of docentries a message list's block relationship refers to should be one. If not, something is
    # terribly wrong.
    if len(message_list.block.docentries) > 1:
        log_error(f"Message list '{listname}' has multiple docentries to its block relationship.")
        raise RouteException("Can't perform deletion at this time. The problem has been logged for admins.")

    # Perform deletion.
    if permanent:
        # If the deletion is (more) permanent, move the admin doc to bin.
        manage_doc = get_doc_or_abort(message_list.manage_doc_id)
        trash_folder: Folder = get_trash_folder()
        move_document(manage_doc, trash_folder)
    # Set the db entry as removed
    message_list.removed = get_current_time()

    if message_list.email_list_domain:
        # A message list has a domain, so we are also looking to delete an email list.
        verify_mailman_connection()
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        delete_email_list(email_list, permanent_deletion=permanent)

    db.session.commit()
    return ok_response()


@messagelist.get("/getlist/<listname>")
def get_list(listname: str) -> Response:
    """Get the information for a message list.

    :param listname: Name of the list
    :return: ListOptions with the list's information.
    """
    if not listname:
        raise NotExist("No message list specified")

    verify_logged_in()
    msg_list = MessageListModel.from_name(listname)
    verify_manage_access(msg_list.block)
    return json_response(msg_list.to_info())


@messagelist.post("/save")
def save_list_options(options: ListInfo) -> Response:
    """Save message list's options.

    :param options: The options to be saved.
    :return: OK response.
    """
    # Check access rights.
    verify_logged_in()
    message_list = MessageListModel.from_name(options.name)
    if not has_manage_access(message_list.block):
        raise RouteException("You need at least a manange access to the list in order to do this action.")

    if message_list.archive_policy != options.archive:
        # TODO: If message list changes its archive policy, the members on the list need to be notified. Insert
        #  messaging here.
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
    set_message_list_default_send_right(message_list, options.default_send_right)
    set_message_list_default_delivery_right(message_list, options.default_delivery_right)
    set_message_list_default_reply_type(message_list, options.default_reply_type)

    db.session.commit()
    return ok_response()


@messagelist.post("/savemembers")
def save_members(listname: str, members: List[MemberInfo]) -> Response:
    """Save the state of existing list members, e.g. send and delivery rights.

    :param listname: The name of the message list where the members will be saved.
    :param members: The members to be saved.
    :return: Response for the client. The Response is a simple ok_response().
    """
    # Check access rights.
    verify_logged_in()
    message_list = MessageListModel.from_name(listname)
    if not has_manage_access(message_list.block):
        raise RouteException("You need at least a manage access to the list to do this action.")

    email_list = None
    if message_list.email_list_domain:
        # If there is a domain configured for a list and the Mailman connection is not configured, we can't continue
        # at this time.
        verify_mailman_connection()
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)

    for member in members:
        db_member = message_list.find_member(member.username, member.email)
        # This if mostly guards against type errors, but what if we legitimely can't find them? They are given from
        # the db in the first place. Is there a reasonable way to communicate this state?
        if db_member:
            set_member_send_delivery(db_member, member.sendRight, member.deliveryRight, email_list=email_list)
            set_message_list_member_removed_status(db_member, member.removed, email_list=email_list)
    db.session.commit()
    return ok_response()


def parse_external_member(external_member_candidate: str) -> Optional[List[str]]:
    """Parse the information of an external member.

    There are two supported ways to give external members. The user can write

        user.userington@domain.fi User Userington
    or
        User Userington <user.userington@domain.fi>

    :param external_member_candidate: A string represeting the external member.
    :return: Return a list of the form [email, name_part_1, name_part_2, ...] if parsing was successful. Otherwise
    return None.
    """
    # Split the name candidate to a list for processing.
    words = external_member_candidate.strip().split(' ')

    # Check if the first word is the email.
    if is_valid_email(words[0]):
        return words

    # If the first word was not an email, then check if the email is at the last word, in angle brackets.
    open_bracket_index = words[-1].find("<")
    closing_bracket_index = words[-1].find(">")
    if open_bracket_index != -1 and closing_bracket_index != -1:
        # Remove angle brackets around the email.
        email = words[-1].strip("<").strip(">")
        if is_valid_email(email):
            return_list = [email]
            return_list.extend(words[0:-1])
            return return_list

    # If we are here, then no applicable version of external member's information was given.
    return None


@messagelist.post("/addmember")
def add_member(member_candidates: List[str], msg_list: str, send_right: bool, delivery_right: bool) -> Response:
    """Add new members to a message list.

    :param member_candidates: Names of member candidates.
    :param msg_list: The message list where we are trying to add new members.
    :param send_right: The send right on a list for all the member candidates.
    :param delivery_right: The delivery right on a list for all the member candidates.
    :return: OK response.
    """
    # Check access right.
    verify_logged_in()
    message_list = MessageListModel.from_name(msg_list)
    if not has_manage_access(message_list.block):
        raise RouteException("You need at least a manage access to the list to do this action.")

    # TODO: Implement checking whether or not users are just added to a list (like they are now) or they are invited
    #  to a list (requires link generation and other things).

    em_list = None
    if message_list.email_list_domain is not None:
        verify_mailman_connection()
        em_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)

    for member_candidate in member_candidates:
        # For user groups and individual users.
        ug = UserGroup.get_by_name(member_candidate.strip())
        if ug is not None:
            # The name belongs to a user group.
            add_new_message_list_group(message_list, ug, send_right, delivery_right, em_list)

        # For external members.
        # If member candidate is not a user, or a user group, then we assume an external member. Add external members.
        external_member = parse_external_member(member_candidate)
        if external_member and em_list:
            if len(external_member) == 1:
                # There is no display name given for external member.
                dname = None
            else:
                # Construct an optional display name by joining all the other words given on the line that are not
                # the email address. Remove possible white space between the email address and the first part of a
                # display name. Other white space within the name is left as is.
                dname = ' '.join(external_member[1:]).lstrip()
            add_message_list_external_email_member(message_list, external_member[0],
                                                   send_right, delivery_right, em_list, display_name=dname)
    db.session.commit()
    return ok_response()


@messagelist.get("/getmembers/<list_name>")
def get_members(list_name: str) -> Response:
    """Get members belonging to a certain list.

    :param list_name: The list where we are querying all the members.
    :return: All the members of a list.
    """
    verify_logged_in()
    msg_list = MessageListModel.from_name(list_name)
    if not has_manage_access(msg_list.block):
        raise RouteException("You are not authorized to see the members of this list.")
    list_members = msg_list.members
    return json_response(list_members)


@messagelist.get("/getgroupmembers/<list_name>")
def get_group_members(list_name: str) -> Response:
    """View function for getting members of groups that are on a message list.

    :param list_name: Message list.
    :return: All members of groups associated in a message list as a list of GroupAndMembers objects.
    """
    # Check rights.
    verify_logged_in()
    message_list = MessageListModel.from_name(list_name)
    if not has_manage_access(message_list.block):
        raise RouteException("Only an owner of this list can see the members of this group.")

    # Get group.
    groups = [member for member in message_list.members if member.is_group()]

    # At this point we assume we have a user that is a TIM user group.
    groups_and_members = []
    for group in groups:
        user_group: UserGroup = group.user_group
        # Create a MemberInfo object for every current user in the group. As these are current members of the user
        # group, removed is None.
        group_members = [MemberInfo(name=user.real_name, username=user.name,
                                    sendRight=group.send_right, deliveryRight=group.delivery_right,
                                    removed=None, email=user.email)
                         for user in user_group.users]
        gm = GroupAndMembers(groupName=user_group.name, members=group_members)
        groups_and_members.append(gm)
    return json_response(groups_and_members)


@messagelist.get("/archive/siblings/<int:message_doc_id>")
def get_sibling_archive_messages(message_doc_id: int) -> Response:
    message_doc = DocEntry.find_by_id(message_doc_id)
    if not message_doc:
        raise RouteException("No document found")
    verify_view_access(message_doc)
    # Only allow jumping to archive messages that the user can view
    docs = message_doc.parent.get_all_documents(
        query_options=load_only(DocEntry.name, DocEntry.id),
        filter_user=get_current_user_object(),
    )
    prev_doc = None
    next_doc = None
    for doc in docs:
        if doc.id == message_doc.id:
            continue
        if doc.id < message_doc.id and (not prev_doc or prev_doc.id < doc.id):
            prev_doc = doc
        if message_doc.id < doc.id and (not next_doc or doc.id < next_doc.id):
            next_doc = doc

    def to_json(d: Optional[DocInfo]) -> Dict[str, Any]:
        return {
            "title": d.title,
            "path": d.path,
        } if d else None

    return json_response({
        "next": to_json(next_doc),
        "prev": to_json(prev_doc)
    })
