from dataclasses import dataclass
from typing import Any
from urllib.error import HTTPError

from mailmanclient import Client, MailingList, Domain, Member
from mailmanclient.restbase.connection import Connection
from marshmallow import EXCLUDE
from sqlalchemy import select, delete

from timApp.messaging.messagelist.listinfo import (
    ListInfo,
    mailman_archive_policy_correlate,
    ArchiveType,
    ReplyToListChanges,
    MessageVerificationType,
)
from timApp.messaging.messagelist.messagelist_models import (
    MessageListModel,
    MessageListExternalMember,
    MessageListMember,
)
from timApp.tim_app import app
from timApp.timdb.sqa import run_sql
from timApp.user.user import User, deleted_user_pattern
from timApp.util.flask.requesthelper import NotExist, RouteException
from timApp.util.logger import log_warning, log_info, log_error
from tim_common.marshmallow_dataclass import class_schema


@dataclass
class MailmanConfig:
    MAILMAN_URL: str | None
    MAILMAN_USER: str | None
    MAILMAN_PASS: str | None
    MAILMAN_UI_LINK_PREFIX: str | None

    def __bool__(self) -> bool:
        return bool(
            self.MAILMAN_URL
            and self.MAILMAN_USER
            and self.MAILMAN_PASS
            and self.MAILMAN_UI_LINK_PREFIX
        )


config: MailmanConfig = class_schema(MailmanConfig)().load(app.config, unknown=EXCLUDE)
_client = (
    Client(config.MAILMAN_URL, config.MAILMAN_USER, config.MAILMAN_PASS)
    if config
    else None
)
"""A client object to utilize Mailman's REST API. If this is None, mailmanclient-library has not been configured for 
use. """


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
        log_message = (
            f"Mailman log: {optional_message}; Mailman returned status code {err}"
        )
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
        log_warning(
            f"Mailman log: Error that should not have happened, happened: {err}"
        )


def verify_mailman_connection() -> None:
    """Verifies if the connection to Mailman is possible. Aborts if connection is not possible due to connection not
    being configured in the first place. Used for operations that are meaningless without configured connection to
    Mailman."""
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


def delete_email_list(
    list_to_delete: MailingList, permanent_deletion: bool = False
) -> None:
    """Delete a mailing list.

    :param permanent_deletion: If True, then the list is permanently gone. If False, perform a soft deletion.
    :param list_to_delete: MailingList object of the email list to be deleted.
    """
    try:
        if permanent_deletion:
            list_to_delete.delete()
        else:
            # Soft deletion.
            freeze_list(list_to_delete)
    except HTTPError as e:
        log_mailman(e, "In delete_email_list()")
        raise


def remove_email_list_membership(
    member: Member, permanent_deletion: bool = False
) -> None:
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
        list_id: str = email_list.rest_data["list_id"]
        template_url: str = app.config["MAILMAN_TEMPLATES_URL"]

        def set_template(template_name: str) -> None:
            url = template_url.format_map(
                {"list_id": list_id, "template_name": template_name}
            )
            email_list.set_template(template_name, url)

        set_template("list:member:regular:footer")
        set_template("list:member:regular:header")
    except HTTPError as e:
        log_mailman(e, "In set_default_templates()")
        raise


def set_email_list_archive_policy(
    email_list: MailingList, archive: ArchiveType
) -> None:
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


def create_new_email_list(list_options: ListInfo, owner: User) -> None:
    """Create a new email list with proper initial options set.

    :param owner: Who owns this list.
    :param list_options: Options for message lists, here we use the options necessary for email list creation.
    :return:
    """
    if list_options.domain:
        verify_name_availability(list_options.name, list_options.domain)
    else:
        log_warning("Tried to create an email list without selected domain part.")
        raise RouteException(
            "Tried to create an email list without selected domain part."
        )

    try:
        domain: Domain = _client.get_domain(list_options.domain)
        email_list: MailingList = domain.create_list(list_options.name)

        set_default_templates(email_list)

        if list_options.list_description:
            set_email_list_description(email_list, list_options.list_description)
        if list_options.list_info:
            set_email_list_info(email_list, list_options.list_info)

        # settings-attribute acts like a dict. Set default settings.
        mlist_settings = email_list.settings

        # Make sure lists aren't advertised by accident by defaulting to not advertising them. Owner switches
        # advertising on if they so choose.
        mlist_settings["advertised"] = False
        # Owners / moderators don't get automatic notifications from changes on their message list. Owner switches
        # this on if necessary.
        mlist_settings["admin_notify_mchanges"] = False
        # Turn off automatic welcome and goodbye messages.
        mlist_settings["send_welcome_message"] = False
        mlist_settings["send_goodbye_message"] = False
        # Set content filtering on, so lists can set pass_extensions on and off. Because allowing attachments is not
        # on by default, add pass_extensions value to block attachments.
        mlist_settings["filter_content"] = True
        mlist_settings["pass_extensions"] = ["no_extension"]
        # Disable the "Should Mailman collapse multipart/alternative to its first part content?" option
        # because it might break sending HTML email with embedded images.
        mlist_settings["collapse_alternatives"] = False

        # This is to force Mailman generate archivers into its db. It fixes a race condition, where creating a new list
        # without proper engineer interface procedures might make duplicate archiver rows in to db, while Mailman's code
        # expects there to be only one archiver row (which results in the db code breaking and the list becoming
        # unusable, at least without manual db fixing). This might be unnecessary at some point in time in the future if
        # the condition is remedied in Mailman Core, but since this line is only needed once on list creation it might
        # be good enough to just leave as is.
        _ = dict(email_list.archivers)

        set_email_list_archive_policy(email_list, list_options.archive)

        # This needs to be the last line aften changing settings, because no changes to settings take effect until
        # save() method is called.
        mlist_settings.save()

        # All lists created through TIM need an owner, and owners need email addresses to control their lists on
        # Mailman.
        email_list.add_owner(owner.email)
        # Add owner automatically as a member of a list, so they receive the posts on the list.
        email_list.subscribe(
            owner.email,
            display_name=owner.real_name,
            pre_approved=True,
            pre_verified=True,
            pre_confirmed=True,
        )
    except HTTPError as e:
        log_mailman(e, "In create_new_email_list()")
        raise


def get_list_ui_link(listname: str, domain: str | None) -> str | None:
    """Get a link for a list to use for advanced email list options and moderation.

    The function assumes that Mailman uses Postorius as its web-UI. There exists no guarantee that other web-UIs would
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
        # Get the list's list id, which is basically its address/name but '@' replaced with a dot.
        list_id: str = mail_list.rest_data["list_id"]
        # Build the hyperlink.
        link = f"{config.MAILMAN_UI_LINK_PREFIX}{list_id}"
        return link
    except HTTPError as e:
        log_mailman(e, "In get_list_ui_link()")
        raise RouteException(
            "Connection to Mailman failed while getting list's UI link."
        )


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


def add_email(
    mlist: MailingList,
    email: str,
    email_owner_pre_confirmation: bool,
    real_name: str | None,
    send_right: bool = True,
    delivery_right: bool = False,
) -> None:
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
    #  might be unnecessary. When the invite to a message list is implemented, check if this is still needed or if the
    #  flag is given the same constant value of True as the other two flags for pre_something.

    # We use pre_verify flag, because we assume email adder knows the address they are adding in. Otherwise they have
    # to verify themselves for Mailman through an additional verification process. Note that this is different from
    # confirming to join a list. We use pre_approved flag, because we assume that who adds an email to a list also
    # wants that email onto a list. Otherwise they would have to afterwards manually moderate their subscription
    # request.
    try:
        new_member = mlist.subscribe(
            email,
            pre_verified=True,
            pre_approved=True,
            pre_confirmed=email_owner_pre_confirmation,
            display_name=real_name,
        )
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
    """Change user's send status on an email list. Send right / status is changed by changing the member's
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


def set_email_list_member_delivery_status(
    member: Member, status: bool, by_moderator: bool = True
) -> None:
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
        mlists: list[MailingList] = checked_domain.get_lists()
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

    If reserved names are not configured, we assume that there are no reserved names.

    :param name_candidate: The name to be compared against reserved names.
    """
    reserved_names = app.config.get("RESERVED_NAMES")
    if reserved_names and name_candidate in reserved_names:
        raise RouteException(
            f"Name '{name_candidate}' is a reserved name and cannot be used."
        )


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


def set_email_list_unsubscription_policy(
    email_list: MailingList, can_unsubscribe_flag: bool
) -> None:
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


def set_email_list_allow_nonmember(
    email_list: MailingList, non_member_message_pass_flag: bool
) -> None:
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


def set_email_list_allow_attachments(
    email_list: MailingList, allow_attachments_flag: bool
) -> None:
    """Set email list allowed attachments.

    :param email_list: The email list where allowed attachment extensions are set.
    :param allow_attachments_flag: For True, set all the allowed extensions for an email list. If False, set the allowed
     extensions to an empty list.
    """
    try:
        if allow_attachments_flag:
            email_list.settings["pass_extensions"] = app.config.get(
                "PERMITTED_ATTACHMENTS"
            )
        else:
            # There might not be a direct option to disallow all attachments to a list. We pass a value we don't expect
            # to find as a file extension. Then the only type of extension that would be allowed is file.no_extensions.
            email_list.settings["pass_extensions"] = ["no_extensions"]
        email_list.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_allow_attachments()")
        raise


def set_email_list_default_reply_type(
    email_list: MailingList, default_reply_type: ReplyToListChanges
) -> None:
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


def set_email_list_verification_mode(
    email_list: MailingList,
    subject_prefix: str | None,
    verification_mode: MessageVerificationType,
) -> None:
    """Set the email list's verification mode.

    :param email_list: The email list where the verification mode is set.
    :param subject_prefix: Subject prefix to use for the list
    :param verification_mode: See MailingListMessageVerificationType and verification_mode variable.
    """

    def update_subject() -> None:
        nonlocal subject_prefix
        if subject_prefix:
            if not subject_prefix.endswith(" "):
                subject_prefix = f"{subject_prefix} "
            email_list.settings["subject_prefix"] = subject_prefix

    try:
        if verification_mode == MessageVerificationType.MUNGE_FROM:
            email_list.settings["dmarc_mitigate_action"] = "munge_from"
            email_list.settings["dmarc_mitigate_unconditionally"] = True
            update_subject()
        elif verification_mode == MessageVerificationType.FORWARD:
            set_default_templates(email_list)
            email_list.settings["dmarc_mitigate_action"] = "no_mitigation"
            email_list.settings["subject_prefix"] = ""
        else:
            email_list.settings["dmarc_mitigate_action"] = "no_mitigation"
            update_subject()
        email_list.settings.save()
    except HTTPError as e:
        log_mailman(e, "In set_email_list_verification_mode()")
        raise


def get_domain_names() -> list[str]:
    """Returns a list of all domain names, that are configured for our instance of Mailman.

    :return: A list of possible domain names.
    """
    try:
        domains: list[Domain] = _client.domains
        domain_names: list[str] = [domain.mail_host for domain in domains]
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
        set_email_list_allow_nonmember(mlist, msg_list.non_member_message_pass)
        mail_list_settings.save()
    except HTTPError as e:
        log_mailman(e, "In unfreeze_list()")
        raise


def find_members_for_address(address: str) -> dict[str, Member]:
    """
    Modified version of
    https://gitlab.com/mailman/mailmanclient/-/blob/509f19b3f666e54f460e7e5f7d2514c758111df3/src/mailmanclient/restobjects/user.py#L60
    to find subscriptions for only the given address
    """
    # Internal mailmanclient member used for authenticated REST calls
    # noinspection PyProtectedMember
    con: Connection = _client._connection
    content: dict[str, Any]
    _, content = con.call("members/find", data={"subscriber": address})
    try:
        return _deduplicate_members(
            [Member(con, entry["self_link"], entry) for entry in content["entries"]]
        )
    except KeyError as e:
        pass
    return {}


def _deduplicate_members(members: list[Member]) -> dict[str, Member]:
    result = {}
    for member in members:
        if member.list_id in result:
            # Deduplicate possible broken subscriptions (Mailman allows them but doesn't like them)
            member.unsubscribe()
        else:
            result[member.list_id] = member
    return result


def update_mailing_list_address(old: str, new: str) -> None:
    if not old or not new:
        return
    if old == new:
        return
    # Don't try to update info for "soft" deleted emails (since the emails are invalid)
    if deleted_user_pattern.match(old) or deleted_user_pattern.match(new):
        return
    if not check_mailman_connection():
        return
    try:
        usr = _client.get_user(old)
        addr = usr.add_address(new, absorb_existing=True)
        addr.verify()
        usr.preferred_address = addr
        old_members = find_members_for_address(old)
        new_members = find_members_for_address(new)

        # Try to pair the old and new members by list_id
        member_pairs: list[tuple[Member, Member]] = []
        for list_id, old_member in old_members.items():
            new_member = new_members.get(list_id, None)
            if new_member:
                member_pairs.append((old_member, new_member))

        # Paired old-new members present a problem: after (old => new) conversion the list would have
        # two members with the same email.
        # Fix this by removing new existing new member and removing all external memberships for that email
        # TODO: This is irreversible, i.e. user changing primary email back doesn't restore old external membership

        for old_member, new_member in member_pairs:
            delete_ids_stmt = (
                select(MessageListExternalMember.id)
                .join(MessageListModel)
                .filter(
                    (MessageListExternalMember.email_address == new)
                    & (MessageListModel.mailman_list_id == new_member.list_id)
                )
            )
            run_sql(
                delete(MessageListExternalMember)
                .where(MessageListExternalMember.id.in_(delete_ids_stmt))
                .execution_options(synchronize_session=False)
            )
            run_sql(
                delete(MessageListMember)
                .where(MessageListMember.id.in_(delete_ids_stmt))
                .execution_options(synchronize_session=False)
            )
            new_member.unsubscribe()

        for member in old_members.values():
            # Mailman objects have dynamic attributes
            # noinspection PyPropertyAccess
            member.address = new
            member.save()
    except HTTPError as e:
        log_mailman(e, f"Could not reroute emails {old} -> {new}")
