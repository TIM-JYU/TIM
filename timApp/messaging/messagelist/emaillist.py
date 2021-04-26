import re
from dataclasses import dataclass
from typing import List, Optional
from urllib.error import HTTPError

from mailmanclient import Client, MailingList, Domain, Member

from timApp.messaging.messagelist.listoptions import ListOptions, mailman_archive_policy_correlate, ArchiveType
from timApp.tim_app import app
from timApp.user.user import User
from timApp.util.flask.requesthelper import NotExist, RouteException
from timApp.util.logger import log_warning, log_info
from tim_common.marshmallow_dataclass import class_schema


@dataclass
class MailmanConfig:
    MAILMAN_URL: Optional[str]
    MAILMAN_USER: Optional[str]
    MAILMAN_PASS: Optional[str]

    def __bool__(self) -> bool:
        return bool(self.MAILMAN_URL and self.MAILMAN_USER and self.MAILMAN_PASS)


config: MailmanConfig = class_schema(MailmanConfig)().load(app.config, unknown="EXCLUDE")
_client = Client(config.MAILMAN_URL, config.MAILMAN_USER, config.MAILMAN_PASS) if config else None
"""
A client object to utilize Mailman's REST API. Poke directly only when necessary, otherwise use via EmailListManager 
class. If this is None, mailmanclient-library has not been configured for use.
"""

# Test mailmanclient's initialization.
if not _client:
    log_warning("No mailman configuration found, no email support will be enabled.")
else:
    log_info("Mailman connection configured.")


# TODO: Test connection somehow?

# TODO: Handle situations where we can't contact Mailman server.

@dataclass
class EmailListManager:
    """Functionality for chosen email list management system Mailman 3. Handels everything else except things
    specific to existing email lists."""

    domains: List[str]
    """Possible domains which can be used with our instance of Mailman."""

    @staticmethod
    def check_name_availability(name_candidate: str, domain: str) -> None:
        """Search for a name from the pool of used email list names.

        Raises a RouteException if no connection was ever established with the Mailman server via mailmanclient.
        Re-raises HTTPError if something went wrong with the query from the server.

        :param domain: Domain to search for lists, which then are used to check name availability.
        :param name_candidate: The name to search for.
        """
        if _client is None:
            raise RouteException("No connection with email list server established.")
        try:
            d = _client.get_domain(domain)
            mlists: List[MailingList] = d.get_lists()
            fqdn_name_candidate = name_candidate + "@" + domain
            for name in [mlist.fqdn_listname for mlist in mlists]:
                if fqdn_name_candidate == name:
                    raise RouteException("Name is already in use.")
        except HTTPError:
            # VIESTIM: Should we just raise the old error or inspect it closer? Now raise old error.
            raise  # "Connection to server failed."

    @staticmethod
    def check_name_requirements(name_candidate: str, domain: str) -> None:
        """Checks name requirements specific for email list.

        If at any point a name requirement check fails, then an exception is raised an carried to the client. If all
        name requirements are met, then succeed silently.

        :param domain: Domain to search for lists.
        :param name_candidate: Name to check against naming rules.
        """
        em = EmailListManager

        # Check name is available.
        em.check_name_availability(name_candidate, domain)

        # Check if name is some reserved name.
        em.check_reserved_names(name_candidate)

        # Check name against name rules. These rules are also checked client-side.
        # VIESTIM: When other message list functionality exists, move this rule check there.
        em.check_name_rules(name_candidate)

        # If we are here, name can be used by the user.

    @staticmethod
    def check_reserved_names(name_candidate: str) -> None:
        """
        Check a name candidate against reserved names, e.g. postmaster. Raises a RouteException if the name candidate
        is a reserved name. If name is not reserved, the method succeeds silently.

        :param name_candidate: The name to be compared against reserved names.
        """
        # TODO: Implement a smarter query for reserved names. Now only compare against simple list for prototyping
        #  purposes. Maybe an external config file for known reserved names or something like that?
        #  Is it possible to query reserved names e.g. from Mailman or it's server?
        reserved_names: List[str] = ["postmaster", "listmaster", "admin"]
        if name_candidate in reserved_names:
            raise RouteException(f"Name {name_candidate} is a reserved name and cannot be used.")

    @staticmethod
    def get_domain_names() -> List[str]:
        """Returns a list of all domain names.

        :return: A list of possible domain names.
        """
        if _client is None:
            return []
        try:
            domains: List[Domain] = _client.domains
            domain_names: List[str] = [domain.mail_host for domain in domains]
            return domain_names
        except HTTPError:
            return []

    @staticmethod
    def check_name_rules(name_candidate: str) -> None:
        """Check if name candidate complies with naming rules. The method raises a RouteException if naming rule is
        violated. If this function doesn't raise an exception, then the name follows naming rules.

        :param name_candidate: What name we are checking.
        """
        # Be careful when checking regex rules. Some rules allow a pattern to exist, while prohibiting others. Some
        # rules prohibit something, but allow other things to exist. If the explanation for a rule is different than
        # the regex, the explanation is more likely to be correct.

        # Name is within length boundaries.
        lower_bound = 5
        upper_bound = 36
        if not (lower_bound < len(name_candidate) < upper_bound):
            raise RouteException(f"Name is not within length boundaries. Name has to be at least {lower_bound} and at "
                                 f"most {upper_bound} characters long.")

        # Name has to start with a lowercase letter.
        start_with_lowercase = re.compile(r"^[a-z]")
        if start_with_lowercase.search(name_candidate) is None:
            raise RouteException("Name has to start with a lowercase letter.")

        # Name cannot have multiple dots in sequence.
        no_sequential_dots = re.compile(r"\.\.+")
        if no_sequential_dots.search(name_candidate) is not None:
            raise RouteException("Name cannot have sequential dots.")

        # Name cannot end in a dot
        if name_candidate.endswith("."):
            raise RouteException("Name cannot end in a dot.")

        # Name can have only these allowed characters. This set of characters is an import from Korppi's character
        # limitations for email list names, and can probably be expanded in the future if desired.
        #     lowercase letters a - z
        #     digits 0 - 9
        #     dot '.'
        #     hyphen '-'
        #     underscore '_'
        # Notice the compliment usage of ^.
        allowed_characters = re.compile(r"[^a-z0-9.\-_]")
        if allowed_characters.search(name_candidate) is not None:
            raise RouteException("Name contains forbidden characters.")

        # Name has to include at least one digit.
        required_digit = re.compile(r"\d")
        if required_digit.search(name_candidate) is None:
            raise RouteException("Name has to include at least one digit.")

        # If we are here, then the name follows all naming rules.


@dataclass
class EmailList:
    """Class to aid with email list specific functionality, such as attribute checking and changes.

    This class is designed to be used when an email list is expected to exits. Think operations like adding
    an email to an existing list etc. For operations other than that, use EmailListManager.
    """

    # VIESTIM: Would it be polite to return something as an indication how the operation went?

    @staticmethod
    def set_archive_type(fqdn_listname: str, archive_status: bool) -> None:
        """
        Set list archiving on or off.

        :param fqdn_listname: The email list's fully qualified domain name, e.g. list1@domain.fi.
        :param archive_status: Models if this list is archived on Mailman's end. If True, all possible archivers are
         used. If False, no archivers are used.
        :return:
        """
        if _client is None:
            return
        try:
            mail_list = _client.get_list(fqdn_listname)
            list_archivers = mail_list.archivers
            for archiver in list_archivers:
                list_archivers[archiver] = archive_status
        except HTTPError:
            pass

    @staticmethod
    def set_notify_owner_on_list_change(listname: str, on_change_flag: bool) -> None:
        if _client is None:
            raise NotExist("No email list server connection.")

        mlist = _client.get_list(listname)
        mlist.settings["admin_notify_mchanges"] = on_change_flag
        mlist.settings.save()
        return

    @staticmethod
    def get_notify_owner_on_list_change(listname: str) -> bool:
        if _client is None:
            raise NotExist("No email list server connection.")
        mlist = _client.get_list(listname)
        return mlist.settings["admin_notify_mchanges"]

    @staticmethod
    def get_archive_type(listname: str) -> bool:
        """
        Get the archive status of a email list.

        :param listname:
        :return: True if email list in question
        """
        if _client is None:
            # TODO: Better return value/error handling here.
            return False
        try:
            mail_list = _client.get_list(listname)
            list_archivers = mail_list.archivers
            # VIESTIM: Here we assume that set_archive_type sets all archivers on or off at the same time. If Mailman
            #  has multiple archivers and they can be set on or off independently, then another solution is required.
            #  We also leverage the fact that Python treats booleans as numbers under the hood.
            archiver_status = [list_archivers[archiver] for archiver in list_archivers]
            if sum(archiver_status) == 0:
                return False
            else:
                return True
        except HTTPError:
            # TODO: Better return value/error handling here.
            return False

    @staticmethod
    def delete_list(fqdn_listname: str) -> str:
        """Delete a mailing list.

        :param fqdn_listname: The fully qualified domain name for the list, e.g. testlist1@domain.fi.
        :return: A string describing how the operation went.
        """
        if _client is None:
            return "There is no connection to Mailman server. No deletion can be attempted."

        try:
            # get_list() may raise HTTPError
            list_to_delete: MailingList = _client.get_list(fqdn_listname)
            list_to_delete.delete()
            return "The list {0} has been deleted.".format(fqdn_listname)
        except HTTPError:
            return "List {0} is not found or connection to server was severed." \
                   " No deletion occured.".format(fqdn_listname)

    @staticmethod
    def get_list_ui_link(listname: str) -> str:
        """
        Get a link for a list to use for advanced email list options and moderation.
        :param listname: The list we are getting the UI link for.
        :return: A Hyperlink for list web UI on the Mailman side.
        """
        if _client is None:
            return ""
        try:
            mail_list = _client.get_list(listname)
            # Get the list's list id, which is basically it's address/name but '@' replaced with a dot.
            list_id: str = mail_list.rest_data["list_id"]
            # VIESTIM: This here is now hardcoded for Postorius Web UI. There might not be a way to just
            #  programmatically get the specific hyperlink for non-TIM email list management needs, but is there a
            #  way to not hard code the Postorius part in (Postorius is Mailman's default web UI and technically
            #  we could switch to a different web UI)?
            # Build the hyperlink.
            link = "https://timlist.it.jyu.fi/postorius/lists/" + list_id
            return link
        except HTTPError:
            return "Connection to Mailman failed while getting list's UI link."

    @staticmethod
    def freeze_list(listname: str) -> None:
        """
        Freeze an email list. No posts are allowed on the list after freezing. Think a course specific email list and
        the course ends, but it's mail archive is kept intact for later potential use. This stops (or at least
        mitigates) that the mail archive on that list changes after the freezing.

        :param listname: The list about the be frozen.
        :return:
        """
        if _client is None:
            return
        try:
            mail_list = _client.get_list(listname)

            # VIESTIM: Another possible way would be to iterate over all members and set their individual moderation
            #  action accordingly. How does Mailman's rule propagation matter here? The rule propagation goes from
            #  user -> member -> list -> system (maybe domain in between list and system). So if member's default
            #  moderation action is 'accept' and we set list's default member action as 'discard', which one wins?
            #  Should we double-tap just to make sure and do both?

            # We freeze a list by simply setting list's moderation to 'discard'. It could also be 'reject'. Holding
            # the messages isn't probably a reasonable choice, since the point of freezing is to not allow posts on
            # the list.
            mail_list_settings = mail_list.settings
            mail_list_settings["default_member_action"] = "discard"
            mail_list_settings["default_nonmember_action"] = "discard"
            mail_list_settings.save()
        except HTTPError:
            return

    @staticmethod
    def unfreeze_list(listname: str) -> None:
        """
        The opposite of freezing a list. Sets some default values for delivery and send status of each member.

        :param listname: The list to bring back into function.
        :return:
        """
        pass

    @staticmethod
    def find_email_lists(email: str) -> List[MailingList]:
        if _client is None:
            return []
        try:
            # VIESTIM: This may or may not be enough. Function find_lists can take optional argument for role on the
            #  list ('member', 'owner' or 'moderator'). This returns them all, but might return duplicates if email
            #  is in different roles in a list. Is it better to ask them from Mailman separated by role, or do role
            #  checking and grouping here?
            lists = _client.find_lists(email)
            return lists
        except HTTPError:
            return []


def remove_email_list_membership(member: Member, permanent_deletion: bool = False) -> None:
    """
    Remove membership from an email list.

    :param member: The membership to be terminated on a list.
    :param permanent_deletion: If True, unsubscribes the user from the list permanently. If False, membership is
     "deleted" in a soft manner by removing delivery and send rights. Membership is kept, but emails from
      member aren't automatically let through nor does the member receive mail from the list.
    """

    if permanent_deletion:
        try:
            member.unsubscribe()
        except HTTPError:
            raise
    else:
        set_email_list_member_send_status(member, False)
        set_email_list_member_delivery_status(member, False)


def set_default_templates(email_list: MailingList) -> None:
    """Set default templates for email list.

    :param: email_list: The email list we are setting templates for.
    """
    # VIESTIM: Sometimes Mailman gets confused how mail is supposed to be interpreted, and with some
    #  email client's different interpretation with Mailman's email coding templates (e.g. list
    #  information footers) may appear as attachments. We fix it by setting header and footer for all new
    #  lists explicitly.

    # VIESTIM: We build the URI in case because not giving one is interpreted by Mailman as deleting the
    #  template form the list.

    list_id = email_list.rest_data['list_id']
    # TODO: Check this URI, it should be plausible if we wished to change default templates to be something other
    #  than empty strings.
    template_base_uri = "http://localhost/postorius/api/templates/list/" \
                        f"{list_id}"
    footer_uri = f"{template_base_uri}/list:member:regular:footer"
    header_uri = f"{template_base_uri}/list:member:regular:header"

    # VIESTIM: These templates don't actually exist, but non-existing templates are substituted with
    #  empty strings and that should still fix the broken coding leading to attachments issue.
    email_list.set_template("list:member:regular:footer", footer_uri)
    email_list.set_template("list:member:regular:header", header_uri)
    return


def set_email_list_archive_policy(email_list: MailingList, archive: ArchiveType) -> None:
    """Set email list's archive policy.

    :param email_list: Email list
    :param archive: What type of archiving is set for message list, and what that means for an email list.
    """
    mlist_settings = email_list.settings
    mm_policy = mailman_archive_policy_correlate[archive]
    if mm_policy == "none":
        # If Archive policy is intented to be 'none', then this list isn't archived at all. Set archive
        # policy and turn off archivers.
        mlist_settings["archive_policy"] = mm_policy
        list_archivers = email_list.archivers
        for archiver in list_archivers:
            list_archivers[archiver] = False
    else:
        # Unless archive policy is intented to be 'none', then we assume archiving to be on by default
        # and we just set the appropriate archive policy.
        mlist_settings["archive_policy"] = mm_policy
    mlist_settings.save()  # This needs to be the last line, otherwise changes won't take effect.
    return


def create_new_email_list(list_options: ListOptions, owner: User) -> None:
    """Create a new email list with proper initial options set.

    :param owner: Who owns this list.
    :param list_options: Options for message lists, here we use the options necessary for email list creation.
    :return:
    """
    if _client is None:
        return
    if EmailListManager.check_name_availability(list_options.listname, list_options.domain):
        try:
            domain: Domain = _client.get_domain(list_options.domain)
            email_list: MailingList = domain.create_list(list_options.listname)
            # VIESTIM: All lists created through TIM need an owner, and owners need email addresses to control
            #  their lists on Mailman.
            email_list.add_owner(owner.email)

            set_default_templates(email_list)

            # settings-attribute acts like a dict.
            mlist_settings = email_list.settings

            # Make sure lists aren't advertised by accident by defaulting to not advertising them. Owner switches
            # advertising on if they so choose.
            mlist_settings["advertised"] = False
            mlist_settings["admin_notify_mchanges"] = list_options.notifyOwnerOnListChange

            set_email_list_description(email_list, list_options.listDescription)
            set_email_list_info(email_list, list_options.listInfo)

            set_email_list_archive_policy(email_list, list_options.archive)

            # This needs to be the last line, because no changes to settings take effect until save-method is
            # called.
            mlist_settings.save()
            return
        except HTTPError:
            # TODO: exceptions to catch: domain doesn't exist, list can't be created, connection to server fails.
            raise
    else:
        # VIESTIM: If a list with this name exists (it shouldn't since it's checked before allowing list creation,
        #  but technically it could if someone can grab the name during list creation process), then what do we do?
        return


def set_email_list_description(mlist: MailingList, new_description: str) -> None:
    """
    Set mailing list's (short) description.
    :param mlist: Email list we wish to set description for.
    :param new_description: A new (short) description for the email list.
    :return:
    """
    mlist.settings["description"] = new_description
    mlist.settings.save()


def get_email_list_description(mlist: MailingList) -> str:
    """
    Get email list's (short) description.
    :param mlist: Email list we wish to get description from.
    :return: A string for email list's description.
    """
    return mlist.settings["description"]


def set_email_list_info(mlist: MailingList, new_info: str) -> None:
    """
    Set email list's info, A.K.A. long description.
    :param mlist: Email list
    :param new_info: New info for the email list.
    :return:
    """
    mlist.settings["info"] = new_info
    mlist.settings.save()


def get_email_list_info(mlist: MailingList) -> str:
    """
    Get email list's info, A.K.A. long description.
    :param mlist: Email list
    :return: Email list's info as a string.
    """
    return mlist.settings["info"]


def get_email_list_by_name(list_name: str, list_domain: str) -> MailingList:
    """Get email list by name.

    :param list_name: List's name.
    :param list_domain: A domain we use to search an email list.
    :return: Return email list as MailingList object
    """
    try:
        mlist = _client.get_list(fqdn_listname=f"{list_name}@{list_domain}")
        return mlist
    except HTTPError:
        # VIESTIM: Should we give some additional information, or are we satisfied with just re-raising the HTTPError
        #  from mailmanclient? This can fail in not getting contact to the server or Mailman not finding a list.
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
    :return:
    """
    # VIESTIM: Check if email belongs to an existing User object? If it does, subscribe the user instead?

    # VIESTIM:
    #  We use pre_verify flag, because we assume email adder knows the address they are adding in. Otherwise
    #  they have to verify themselves for Mailman. Note that this is different from confirming to join a list.
    #  We use pre_approved flag, because we assume that who adds an email to a list also wants that email onto
    #  a list. Otherwise they would have to afterwards manually moderate their subscription request.
    try:
        new_member = mlist.subscribe(email,
                                     pre_verified=True,
                                     pre_approved=True,
                                     pre_confirmed=email_owner_pre_confirmation,
                                     display_name=real_name)
        # Set member's send and delivery rights to email list.
        set_email_list_member_send_status(new_member, send_right)
        set_email_list_member_delivery_status(new_member, delivery_right)
    except HTTPError:
        raise


def set_email_list_member_send_status(member: Member, status: bool) -> None:
    """ Change user's send status on an email list. Send right / status is changed by changing the member's
    moderation status.

    This function can fail if connection to Mailman is lost.

    :param member: Member who is having their send status changed.
    :param status: A value that determines if option is 'enabled' or 'disabled'. If True, the member can send messages
    (past moderation) to the email list. If False, then all email from member will be rejected.
    """
    if status:
        # This could also be e.g. "discard", but then there would be no information whether or not the email ever
        # made it to moderation.
        member.moderation_action = "accept"
    else:
        member.moderation_action = "reject"
    # Changing the moderation_action requires saving, otherwise change won't take effect.
    try:
        member.save()
        return
    except HTTPError:
        # Saving can fail if connection is lost to Mailman or it's server.
        raise


def set_email_list_member_delivery_status(member: Member, status: bool, by_moderator: bool = True) -> None:
    """Change email list's member's delivery status on a list.

    This function can fail if connection to Mailman is lost.

    :param member: Member who is having their delivery status changed.
    :param status: If True, then this member receives email list's messages. If false, member does not receive messages
    from the email list.
    :param by_moderator: Who initiated the change in delivery right. If True, then the change was initiated by a
    moderator or owner. If False, then the change was initiated by the member themselves.
    """
    # Viestim: This is just an idea how to go about changing user's delivery options. There exists
    #  frustratingly little documentation about this kind of thing, so this might not work. The idea
    #  originates from
    #  https://docs.mailman3.org/projects/mailman/en/latest/src/mailman/handlers/docs/owner-recips.html
    #  More information also at
    #  https://gitlab.com/mailman/mailman/-/blob/master/src/mailman/interfaces/member.py
    if status:
        member.preferences["delivery_status"] = "enabled"
    else:
        if by_moderator:
            member.preferences["delivery_status"] = "by_moderator"
        else:
            member.preferences["delivery_status"] = "by_user"
    # If changed, preferences have to be saved for them to take effect on Mailman.
    try:
        member.preferences.save()
        return
    except HTTPError:
        # Saving can fail if connection is lost to Mailman or it's server.
        raise


def get_email_list_member_delivery_status(member: Member) -> bool:
    """Get member's delivery status.

    :param member: Member who's delivery status / right on a list we are interested in.
    :return: A string 'enabled' or 'disabled'.
    """
    member_preferences = member.preferences
    if member_preferences["delivery_status"] == "enabled":
        return True
    # VIESTIM: If delivery status is "by_bounces", then something is wrong with member's email address as it cannot
    #  properly receive email. Is there anything meaningful we can do about it here?
    if member_preferences["delivery_status"] in ["by_user", "by_moderator", "by_bounces"]:
        return False
    # If we are here, something has gone terribly wrong.
    # VIESTIM: Is this logging worthy? If yes, what severity?
    raise RouteException(f"Member {member.address} has an invalid delivery status assigned to them.")


def get_email_list_member_send_status(member: Member) -> bool:
    """Get email list's member's send status / right in an email list.

    :param member: Member who's send status we wish to know.
    :return: Return True, if member has a send right to a list (moderation action is set as "accept"). Otherwise return
    False.
    """
    if member.moderation_action == "accept":
        return True
    if member.moderation_action in ["discard", "reject", "hold"]:
        return False
    # If we are here, something has gone terribly wrong.
    # VIESTIM: Is this logging worthy? If yes, what severity?
    raise RouteException(f"Member {member.address} has an invalid send status assigned to them.")
