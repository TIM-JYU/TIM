from dataclasses import dataclass
from typing import List, Optional
from urllib.error import HTTPError

from mailmanclient import Client, MailingList, Domain, Member

from timApp.messaging.messagelist.listoptions import ListOptions, mailman_archive_policy_correlate, ArchiveType, \
    ReplyToListChanges
from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.tim_app import app
from timApp.user.user import User
from timApp.util.flask.requesthelper import NotExist, RouteException
from timApp.util.logger import log_warning, log_info, log_error
from tim_common.marshmallow_dataclass import class_schema


@dataclass
class MailmanConfig:
    MAILMAN_URL: Optional[str]
    MAILMAN_USER: Optional[str]
    MAILMAN_PASS: Optional[str]
    MAILMAN_UI_LINK_PREFIX: Optional[str]

    def __bool__(self) -> bool:
        return bool(self.MAILMAN_URL and self.MAILMAN_USER and self.MAILMAN_PASS and self.MAILMAN_UI_LINK_PREFIX)


config: MailmanConfig = class_schema(MailmanConfig)().load(app.config, unknown="EXCLUDE")
_client = Client(config.MAILMAN_URL, config.MAILMAN_USER, config.MAILMAN_PASS) if config else None
"""A client object to utilize Mailman's REST API. If this is None, mailmanclient-library has not been configured for 
use. """

# Test mailmanclient's initialization on TIM's boot up.
if not _client:
    log_warning("No mailman configuration found, no email support will be enabled.")
else:
    log_info("Mailman connection configured.")


def log_mailman(err: HTTPError, optional_message: str = "") -> None:
    """Log potentially troublesome Mailman activity when mailmanclient raises HTTPErrors.

    mailmanclient library raises liburl HTTPError messages for non 2xx status code responses for backward
    compatibility. Some of these indicate actual trouble that should be investigated, and some are just information
    about the operation (e.g. subscribe() method for MailinList objects raises HTTPError with a code 409 if the
    address we are trying to subscribe already exists on the list).

    The general idea is to use log_error() for 5Xxx status codes and log_warning() for 4xx status codes. Technically
    3xx codes should not happen for TIM, but they get log_info().

    :param err: The HTTPError to be logged.
    :param optional_message: Optional, additional message for logging.
    """
    code_category = err.code // 100
    # If an optional message is given, it sets a slightly different format for the message.
    if optional_message:
        log_message = f"Mailman log: {optional_message}; Mailman returned status code {err}"
    else:
        log_message = f"Mailman log: Mailman returned status code {err}"
    # Check for 5xx, 4xx and 3xx status codes, they are logged with different severity.
    if code_category == 5:
        log_error(log_message)
    elif code_category == 4:
        log_warning(log_message)
    elif code_category == 3:
        log_info(log_message)
    else:
        log_warning(f"Mailman log: Error that should not have happened, happened: {err}")


def verify_mailman_connection() -> None:
    """Verifies if the connection to Mailman is possible. Aborts if connection is not possible due to connection not
    being configured in the first place. Used for operations that are meaningless without configured connection to
    Mailman. """
    if not _client:
        raise NotExist("No connection to Mailman configured.")


def check_mailman_connection() -> bool:
    """Checks if the connection to Mailman is possible. Used for operations where the ability to connect to Mailman
    is optional, and other meaningful operations can resume in case the connection is not available.

    :return: True if connection is possible, i.e. _client is configured. Otherwise False.
    """
    if not _client:
        return False
    return True


def set_notify_owner_on_list_change(mlist: MailingList, on_change_flag: bool) -> None:
    """Set email list's notify owner on list change change flag.

    :param mlist: Email list where the flag is set.
    :param on_change_flag: For True, set the notification flag on and then the changes on a list will send
    notifications from Mailman. For False, set the flag off and then the email list will not send notifications
    from Mailman.
    """
    try:
        mlist.settings["admin_notify_mchanges"] = on_change_flag
        mlist.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_notify_owner_on_list_change()")
        raise


def delete_email_list(fqdn_listname: str, permanent_deletion: bool = False) -> None:
    """Delete a mailing list.

    :param permanent_deletion: If True, then the list is permanently gone. If False, perform a soft deletion.
    :param fqdn_listname: The fully qualified domain name for the list, e.g. testlist1@domain.fi.
    """
    try:
        list_to_delete: MailingList = _client.get_list(fqdn_listname)
        if permanent_deletion:
            list_to_delete.delete()
        else:
            # Soft deletion.
            freeze_list(list_to_delete)
    except HTTPError as e:
        log_mailman(e, "In delete_email_list()")
        raise


def remove_email_list_membership(member: Member, permanent_deletion: bool = False) -> None:
    """Remove membership from an email list.

    :param member: The membership to be terminated on a list.
    :param permanent_deletion: If True, unsubscribes the user from the list permanently. If False, membership is
    "deleted" in a soft manner by removing delivery and send rights, conforming to TIM's policy in deleting objects.
    Membership is kept, but emails from member aren't automatically let through nor does the member receive mail from
    the list.
    """
    try:
        if permanent_deletion:
            member.unsubscribe()
        else:
            set_email_list_member_send_status(member, False)
            set_email_list_member_delivery_status(member, False)
    except HTTPError as e:
        log_mailman(e, "In remove_email_list_membership()")
        raise


def set_default_templates(email_list: MailingList) -> None:
    """Set default templates for email list.

    Sometimes Mailman gets confused how mail is supposed to be interpreted, and with some  email client's different
    interpretation with Mailman's email coding templates (e.g. list information footers) may appear as attachments.
    We fix it by setting header and footer for all new lists explicitly.

    :param: email_list: The email list we are setting templates for.
    """

    # Build the URI in case because not giving one is interpreted by Mailman as deleting the template form the list.
    try:

        list_id = email_list.rest_data['list_id']
        template_base_uri = "http://localhost/postorius/api/templates/list/" \
                            f"{list_id}"
        footer_uri = f"{template_base_uri}/list:member:regular:footer"
        header_uri = f"{template_base_uri}/list:member:regular:header"

        # These templates don't actually exist, but non-existing templates are substituted with empty strings and that
        # should still fix the broken coding leading to attachments issue.
        email_list.set_template("list:member:regular:footer", footer_uri)
        email_list.set_template("list:member:regular:header", header_uri)
    except HTTPError as e:
        log_mailman(e, "In set_default_templates()")
        raise


def set_email_list_archive_policy(email_list: MailingList, archive: ArchiveType) -> None:
    """Set email list's archive policy.

    :param email_list: Email list
    :param archive: What type of archiving is set for message list, and what that means for an email list.
    """
    try:
        mlist_settings = email_list.settings
        mm_policy = mailman_archive_policy_correlate[archive]
        mlist_settings["archive_policy"] = mm_policy
        mlist_settings.save()  # This needs to be the last line, otherwise changes won't take effect.
    except HTTPError as e:
        log_mailman(e, "In set_email_list_archive_policy()")
        raise


def create_new_email_list(list_options: ListOptions, owner: User) -> None:
    """Create a new email list with proper initial options set.

    :param owner: Who owns this list.
    :param list_options: Options for message lists, here we use the options necessary for email list creation.
    :return:
    """
    if list_options.domain:
        verify_name_availability(list_options.name, list_options.domain)
    else:
        log_warning("Tried to create an email list without selected domain part.")
        raise RouteException("Tried to create an email list without selected domain part.")

    try:
        domain: Domain = _client.get_domain(list_options.domain)
        email_list: MailingList = domain.create_list(list_options.name)
        # All lists created through TIM need an owner, and owners need email addresses to control their lists on
        # Mailman.
        email_list.add_owner(owner.email)
        # Add owner automatically as a member of a list.
        email_list.subscribe(owner.email, display_name=owner.real_name, pre_approved=True, pre_verified=True,
                             pre_confirmed=True)

        set_default_templates(email_list)

        # settings-attribute acts like a dict.
        mlist_settings = email_list.settings

        # Make sure lists aren't advertised by accident by defaulting to not advertising them. Owner switches
        # advertising on if they so choose.
        mlist_settings["advertised"] = False
        # Ownerss / moderators don't get automatic notifications from changes on their message list. Owner switches
        # this on if necessary.
        mlist_settings["admin_notify_mchanges"] = False

        if list_options.list_description:
            set_email_list_description(email_list, list_options.list_description)
        if list_options.list_info:
            set_email_list_info(email_list, list_options.list_info)

        # This is to force Mailman generate archivers into it's db. It exists is to fix a race condition,
        # where creating a new list without proper engineer interface procedures might make duplicate archiver rows
        # in to db, while Mailman's code expects there to be only one archiver row (which results in the db code
        # breaking and the list becoming unusable, at least without manual db fixing). This might be unnecessary at
        # some point in time in the future if the condition is remedied in Mailman Core, but since this line is only
        # needed once on list creation it might be good enough to just leave as is.
        _ = dict(email_list.archivers)

        set_email_list_archive_policy(email_list, list_options.archive)

        # This needs to be the last line, because no changes to settings take effect until save() method is
        # called.
        mlist_settings.save()
    except HTTPError as e:
        log_mailman(e, "In create_new_email_list()")
        raise


def get_list_ui_link(listname: str, domain: Optional[str]) -> Optional[str]:
    """Get a link for a list to use for advanced email list options and moderation.

    The function assumes that Mailman uses Postorius as it's web-UI. There exists no guarantee that other web-UIs would
    use the exact form for their links. If Postorius is changed to some other web-UI, this needs to be updated.

    :param listname: The list we are getting the UI link for.
    :param domain: Domain for the list.
    :return: A Hyperlink for list web UI on the Mailman side. If there is no email list for (parameter domain is None)
    then return None. Return None if no connection to Mailman is configured.
    """
    try:
        if domain is None or not config.MAILMAN_UI_LINK_PREFIX:
            return None
        if _client is None:
            return None
        mail_list = _client.get_list(f"{listname}@{domain}")
        # Get the list's list id, which is basically it's address/name but '@' replaced with a dot.
        list_id: str = mail_list.rest_data["list_id"]
        # Build the hyperlink.
        link = f"{config.MAILMAN_UI_LINK_PREFIX}{list_id}"
        return link
    except HTTPError as e:
        log_mailman(e, "In get_list_ui_link()")
        raise RouteException("Connection to Mailman failed while getting list's UI link.")


def set_email_list_description(mlist: MailingList, new_description: str) -> None:
    """Set mailing list's (short) description.

    :param mlist: Email list we wish to set description for.
    :param new_description: A new (short) description for the email list.
    """
    try:
        mlist.settings["description"] = new_description
        mlist.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_description()")
        raise


def set_email_list_info(mlist: MailingList, new_info: str) -> None:
    """Set email list's info, A.K.A. long description.

    :param mlist: Email list where the info is set.
    :param new_info: New info for the email list.
    """
    try:
        mlist.settings["info"] = new_info
        mlist.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_info()")
        raise


def get_email_list_by_name(list_name: str, list_domain: str) -> MailingList:
    """Get email list by name.

    :param list_name: List's name.
    :param list_domain: A domain we use to search an email list.
    :return: Return email list as an MailingList object.
    """
    try:
        mlist = _client.get_list(fqdn_listname=f"{list_name}@{list_domain}")
        return mlist
    except HTTPError as e:
        log_mailman(e, "In get_email_list_by_name()")
        raise


def add_email(mlist: MailingList, email: str, email_owner_pre_confirmation: bool, real_name: Optional[str],
              send_right: bool = True, delivery_right: bool = False) -> None:
    """Add a new email to a email list.

    :param mlist: Email list where a new member is being added.
    :param email: The email being added. Email address has to be validated before calling this function.
    :param email_owner_pre_confirmation: Whether the email's owner has to confirm them joining an email list. For True,
    no confirmation is needed by the email's owner. For False, Mailman send's a confirmation mail for email's owner to
    join the list.
    :param real_name: Name associated with the email.
    :param send_right: Whether email can send mail to the list. For True, then can send messages and for False they
    can't.
    :param delivery_right: Whether email list delivers mail to email. For True, mail is delivered to email. For False,
    no mail is delivered.
    """
    # TODO: The email_owner_pre_confirmation flag was made available for caller with the idea that when inviting to
    #  message list would be implemented, this would control whether the new user was invited or directly added.
    #  However, inviting will most likely be implemented with other means than Mailman's invite system, so the flag
    #  might be unnecessary. When the invite to a messge list is implemented, check if this is still needed or if the
    #  flag is given the same constant value of True as the other two flags for pre_something.

    # We use pre_verify flag, because we assume email adder knows the address they are adding in. Otherwise they have
    # to verify themselves for Mailman through an additional verification process. Note that this is different from
    # confirming to join a list. We use pre_approved flag, because we assume that who adds an email to a list also
    # wants that email onto a list. Otherwise they would have to afterwards manually moderate their subscription
    # request.
    try:
        new_member = mlist.subscribe(email,
                                     pre_verified=True,
                                     pre_approved=True,
                                     pre_confirmed=email_owner_pre_confirmation,
                                     display_name=real_name)
        # Set member's send and delivery rights to email list.
        set_email_list_member_send_status(new_member, send_right)
        set_email_list_member_delivery_status(new_member, delivery_right)
    except HTTPError as e:
        if e.code == 409:
            # With code 409, Mailman indicates that the member is already in the list. We assume that a member has
            # been soft removed previously, and is now re-added to email list. Set send and delivery rights.
            member = get_email_list_member(mlist, email)
            set_email_list_member_send_status(member, send_right)
            set_email_list_member_delivery_status(member, delivery_right)
        else:
            log_mailman(e, "In add_email()")
            raise


def set_email_list_member_send_status(member: Member, status: bool) -> None:
    """ Change user's send status on an email list. Send right / status is changed by changing the member's
    moderation status.

    This function can fail if connection to Mailman is lost.

    :param member: Member who is having their send status changed.
    :param status: A value that determines if option is 'enabled' or 'disabled'. If True, the member can send messages
    (past moderation) to the email list. If False, then all email from member will be rejected.
    """
    try:
        if status:
            member.moderation_action = "accept"
        else:
            member.moderation_action = "reject"
        member.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_member_send_status()")
        raise


def set_email_list_member_delivery_status(member: Member, status: bool, by_moderator: bool = True) -> None:
    """Change email list's member's delivery status on a list.

    This function can fail if connection to Mailman is lost.

    :param member: Member who is having their delivery status changed.
    :param status: If True, then this member receives email list's messages. If false, member does not receive messages
    from the email list.
    :param by_moderator: Who initiated the change in delivery right. If True, then the change was initiated by a
    moderator or owner of a list. If False, then the change was initiated by the member themselves.
    """
    try:
        if status:
            member.preferences["delivery_status"] = "enabled"
        else:
            if by_moderator:
                member.preferences["delivery_status"] = "by_moderator"
            else:
                member.preferences["delivery_status"] = "by_user"
        member.preferences.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_member_delivery_status()")
        raise


def get_email_list_member_delivery_status(member: Member) -> bool:
    """Get email list member's delivery status.

    :param member: Member who's delivery status / right on a list we are interested in.
    :return: True if the member has an equivalent of a delivery right. Otherwise return False.
    """
    try:
        member_preferences = member.preferences
        if member_preferences["delivery_status"] == "enabled":
            return True
        # If delivery status is "by_bounces", then something is wrong with member's email address as it cannot
        # properly receive email. Fot this context, it's still taken as the member not having a delivery right.
        elif member_preferences["delivery_status"] in ["by_user", "by_moderator", "by_bounces"]:
            return False
        # If we are here, something has gone terribly wrong.
        log_warning(f"Member {member.address} has an invalid delivery status assigned to them.")
        raise RouteException(f"Member {member.address} has an invalid delivery status assigned to them.")
    except HTTPError as e:
        log_mailman(e, "In get_email_list_member_delivery_status()")
        raise


def get_email_list_member_send_status(member: Member) -> bool:
    """Get email list's member's send status / right in an email list.

    :param member: Member who's send status we wish to know.
    :return: Return True, if member has a send right to a list (moderation action is set as "accept"). Return
    False if moderation action is one of 'discard', 'reject', or 'hold'.
    """
    try:
        if member.moderation_action == "accept":
            return True
        if member.moderation_action in ["discard", "reject", "hold"]:
            return False
        # If we are here, something has gone terribly wrong.
        log_warning(f"Member {member.address} has an invalid send status assigned to them.")
        raise RouteException(f"Member {member.address} has an invalid send status assigned to them.")
    except HTTPError as e:
        log_mailman(e, "In get_email_list_member_send_status()")
        raise


def verify_emaillist_name_requirements(name_candidate: str, domain: str) -> None:
    """Check email list's name requirements. General message list name requirement checks are assumed to be passed
    at this point and that those requirements encompass email list name requirements.

    :param name_candidate: A possible name for an email list name to check.
    :param domain: Domain where name availability is to be checked.
    """
    verify_name_availability(name_candidate, domain)
    verify_reserved_names(name_candidate)


def verify_name_availability(name_candidate: str, domain: str) -> None:
    """Search for a name from the pool of used email list names.

    Raises a RouteException if no connection was ever established with the Mailman server via mailmanclient.

    :param domain: Domain to search for lists, which then are used to check name availability.
    :param name_candidate: The name to search for.
    """
    try:
        checked_domain = _client.get_domain(domain)
        mlists: List[MailingList] = checked_domain.get_lists()
        fqdn_name_candidate = f"{name_candidate}@{domain}"
        for name in [mlist.fqdn_listname for mlist in mlists]:
            if fqdn_name_candidate == name:
                raise RouteException("Name is already in use.")
    except HTTPError as e:
        log_mailman(e, "In check_name_availability()")
        raise


def verify_reserved_names(name_candidate: str) -> None:
    """Check a name candidate against reserved names, e.g. postmaster.

    Raises a RouteException if the name candidate is a reserved name. If name is not reserved, the method completes
    silently.

    :param name_candidate: The name to be compared against reserved names.
    """
    # TODO: Implement a smarter query for reserved names. Now only compare against simple list for prototyping
    #  purposes. Maybe an external config file for known reserved names or something like that?
    #  Is it possible to query reserved names e.g. from Mailman or it's server?
    reserved_names: List[str] = ["postmaster", "listmaster", "admin"]
    if name_candidate in reserved_names:
        raise RouteException(f"Name '{name_candidate}' is a reserved name and cannot be used.")


def get_email_list_member(mlist: MailingList, email: str) -> Member:
    """Get a Member object with an email address from a MailingList object (i.e. from an email list).

    :param mlist: MailingList object, the email list in question.
    :param email: Email used to find the member in an email list.
    :return: Return a Member object belonging to an email address on an email list.
    """
    try:
        member = mlist.get_member(email)
        return member
    except HTTPError as e:
        log_mailman(e, "In get_email_list_member()")
        raise


def set_email_list_unsubscription_policy(email_list: MailingList, can_unsubscribe_flag: bool) -> None:
    """Set the unsubscription policy of an email list.

    :param email_list: The email list where the policy is to be set.
    :param can_unsubscribe_flag: For True, then set the policy as 'confirm_then_moderate'. For False, set the policy as
    'confirm'.
    """
    # mailmanclient exposes an 'unsubscription_policy' setting, that follows SubscriptionPolicy enum values, see
    # https://gitlab.com/mailman/mailman/-/blob/master/src/mailman/interfaces/mailinglist.py for details.
    try:
        if can_unsubscribe_flag:
            email_list.settings["unsubscription_policy"] = "confirm"
        else:
            email_list.settings["unsubscription_policy"] = "confirm_then_moderate"
        email_list.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_unsubscription_policy()")
        raise


def set_email_list_subject_prefix(email_list: MailingList, subject_prefix: str) -> None:
    """Set the subject prefix for an email list.

    :param email_list: Email list where the subject prefix is to be set.
    :param subject_prefix: The prefix set for email list's subject.
    """
    try:
        email_list.settings["subject_prefix"] = subject_prefix
        email_list.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_subject_prefix()")
        raise


def set_email_list_only_text(email_list: MailingList, only_text: bool) -> None:
    """Set email list to only text mode. Affects new email sent to list and HyperKitty archived messages.

    :param email_list: Email list which is to be set into text only mode.
    :param only_text: A boolean flag controlling list rendering mode. For True, the list is in an only text mode.
    For False, the list is not on an only text mode, and other rendering (e.g. HTML) is allowed.
    """
    try:
        email_list.settings["convert_html_to_plaintext"] = only_text
        # The archive_rendering_mode setting mainly has an effect on HyperKitty. If the archiver on Mailman is something
        # else, this might have no effect. It is still exposed on mailmanclient, so it should not be harmful either.
        if only_text:
            email_list.settings["archive_rendering_mode"] = "text"
        else:
            email_list.settings["archive_rendering_mode"] = "markdown"
        email_list.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_only_text()")
        raise


def set_email_list_non_member_message_pass(email_list: MailingList, non_member_message_pass_flag: bool) -> None:
    """Set email list's non member (message pass) action.

    :param email_list: The email list where the non member message pass action is set.
    :param non_member_message_pass_flag: For True, set the default non member moderation action as 'accept'. For False,
    set the default non member moderation action as 'hold'
    """
    try:
        if non_member_message_pass_flag:
            email_list.settings["default_nonmember_action"] = "accept"
        else:
            email_list.settings["default_nonmember_action"] = "hold"
        email_list.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_non_member_message_pass()")
        raise


def set_email_list_allow_attachments(email_list: MailingList, allow_attachments_flag: bool) -> None:
    """Set email list allowed attachments.

    :param email_list: The email list where allowed attachment extensions are set.
    :param allow_attachments_flag: For True, set all the allowed extensions for an email list. If False, set the allowed
     extensions to an empty list.
    """
    try:
        if allow_attachments_flag:
            email_list.settings["pass_extensions"] = app.config.get("PERMITTED_ATTACHMENTS")
        else:
            # There might not be a direct option to disallow all attachments to a list. We pass a value we don't expect
            # to find as a file extension. Then the only type of extension that would be allowed is file.no_extensions.
            email_list.settings["pass_extensions"] = ["no_extensions"]
        email_list.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_allow_attachments()")
        raise


def set_email_list_default_reply_type(email_list: MailingList, default_reply_type: ReplyToListChanges) -> None:
    """Set the email list's default reply type, i.e. perform Reply-To munging.

    :param email_list: The email list where the reply type is set.
    :param default_reply_type: See ReplyToListChanges and reply_to_munging variable.
    """
    try:
        email_list.settings["reply_goes_to_list"] = default_reply_type.value
        email_list.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_default_reply_type()")
        raise


def get_domain_names() -> List[str]:
    """Returns a list of all domain names, that are configured for our instance of Mailman.

    :return: A list of possible domain names.
    """
    try:
        domains: List[Domain] = _client.domains
        domain_names: List[str] = [domain.mail_host for domain in domains]
        return domain_names
    except HTTPError as e:
        log_mailman(e, "In get_domain_names()")
        raise


def freeze_list(mlist: MailingList) -> None:
    """Freeze an email list. No posts are allowed on the list after freezing (owner might be an exception).

    Think a course specific email list and the course ends, but mail archive is kept intact for later potential use.
    This stops (or at least mitigates) that the mail archive on that list changes after the freezing.

    :param mlist: The list about the be frozen.
    """
    try:
        for member in mlist.members:
            # All members have their send and delivery rights revoked. We need this, because individual members
            # settings, when set, take precedence over list settings.
            set_email_list_member_delivery_status(member, False, by_moderator=True)
            set_email_list_member_send_status(member, False)

        # Also set list's default moderation actions for good measure.
        mail_list_settings = mlist.settings
        mail_list_settings["default_member_action"] = "reject"
        mail_list_settings["default_nonmember_action"] = "reject"
        mail_list_settings.save()
    except HTTPError as e:
        log_mailman(e, "In freeze_list()")
        raise


def unfreeze_list(mlist: MailingList, msg_list: MessageListModel) -> None:
    """The opposite of freezing a list.

    Sets default values for delivery and send status of each member, depending on message lists options.

    :param msg_list: The message list the email list belongs to.
    :param mlist: The list to bring back into function.
    """
    # Not in use at the moment.
    try:
        mail_list_settings = mlist.settings
        mail_list_settings["default_member_action"] = "accept"
        set_email_list_non_member_message_pass(mlist, msg_list.non_member_message_pass)
        mail_list_settings.save()
    except HTTPError as e:
        log_mailman(e, "In unfreeze_list()")
        raise
