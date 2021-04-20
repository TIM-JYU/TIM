import re
from dataclasses import dataclass
from typing import List, Tuple, Optional
from urllib.error import HTTPError

from mailmanclient import Client, MailingList, Domain

from timApp.messaging.messagelist.listoptions import ListOptions, mailman_archive_policy_correlate
from timApp.tim_app import app
from timApp.util.flask.requesthelper import NotExist
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
    def check_name_availability(name_candidate: str, domain: str) -> Tuple[Optional[bool], str]:
        """Search for a name from the pool of used email list names.

        :param domain: Domain to search for lists, which then are used to check name availability.
        :param name_candidate: The name to search for.
        :return: If we cannot get data from Mailman, return None and an explanation string. Return True if name is
        available. Return False if name was not available and a string to specify why.
        """
        if _client is None:
            return None, "No connection with email list server established."
        try:
            d = _client.get_domain(domain)
            mlists: List[MailingList] = d.get_lists()
            fqdn_name_candidate = name_candidate + "@" + domain
            for name in [mlist.fqdn_listname for mlist in mlists]:
                if fqdn_name_candidate == name:
                    return False, "Name is already in use."
            return True, "Name is available."
        except HTTPError:
            return None, "Connection to server failed."

    @staticmethod
    def check_name_requirements(name_candidate: str, domain: str) -> Tuple[Optional[bool], str]:
        """Checks name requirements specific for email list.

        :param domain: Domain to search for lists.
        :param name_candidate: Name to check against naming rules.
        :return: Return A tuple. None if connection to Mailman failed. Return True if all name requirements are met.
         Otherwise return False. In all cases, return an explanatory string as the second part of the tuple.
        """
        em = EmailListManager

        # Check name is available.
        available, availability_explanation = em.check_name_availability(name_candidate, domain)
        if not available:
            return available, availability_explanation

        # Check if name is some reserved name.
        not_reserved, reserved_explanation = em.check_reserved_names(name_candidate)
        if not not_reserved:
            return not_reserved, reserved_explanation

        # Check name against name rules. These rules are also checked client-side.
        # VIESTIM: When other message list functionality exists, move this rule check there.
        within_rules, rule_explanation = em.check_name_rules(name_candidate)
        if not within_rules:
            return within_rules, rule_explanation

        return True, "Ok."

    @staticmethod
    def check_reserved_names(name_candidate: str) -> Tuple[bool, str]:
        """
        Check a name candidate against reserved names, e.g. postmaster.

        :param name_candidate: The name to be compared against reserved names.
        :return: Return True if name is not among reserved names. Otherwise return False.
        """
        # TODO: Implement a smarter query for reserved names. Now only compare against simple list for prototyping
        #  purposes. Maybe an external config file for known reserved names or something like that?
        #  Is it possible to query reserved names e.g. from Mailman or it's server?
        reserved_names: List[str] = ["postmaster", "listmaster", "admin"]
        if name_candidate in reserved_names:
            return False, "Name {0} is a reserved name.".format(name_candidate)
        else:
            return True, "Name is not reserved and can be used."

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
    def create_new_list(list_options: ListOptions) -> None:
        """Create a new email list with proper initial options set.

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
                email_list.add_owner(list_options.ownerEmail)

                # VIESTIM: Sometimes Mailman gets confused how mail is supposed to be interpreted, and with some
                #  email client's different interpretation with Mailman's email coding templates (e.g. list
                #  information footers) may appear as attachments. We fix it by setting header and footer for all new
                #  lists explicitly.

                # VIESTIM: We build the URI in case because not giving one is interpreted by Mailman as deleting the
                #  template form the list.

                list_id = email_list.rest_data['list_id']
                template_base_uri = "http://localhost/postorius/api/templates/list/" \
                                    f"{list_id}"

                footer_uri = f"{template_base_uri}/list:member:regular:footer"
                header_uri = f"{template_base_uri}/list:member:regular:header"

                # VIESTIM: These templates don't actually exist, but non-existing templates are substituted with
                #  empty strings and that should still fix the broken coding leading to attachments issue.
                email_list.set_template("list:member:regular:footer", footer_uri)
                email_list.set_template("list:member:regular:header", header_uri)

                # settings-attribute is a dict.
                mlist_settings = email_list.settings

                # Make sure lists aren't advertised by accident by defaulting to not advertising them. Owner switches
                # advertising on if they so choose.
                mlist_settings["advertised"] = False
                mlist_settings["admin_notify_mchanges"] = list_options.notifyOwnerOnListChange
                set_email_list_description(email_list, list_options.listDescription)
                set_email_list_info(email_list, list_options.listInfo)

                mm_policy = mailman_archive_policy_correlate[list_options.archive]
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
                # This needs to be the last line, because no changes to settings take effect until save-method is
                # called.
                mlist_settings.save()
                return
            except HTTPError:
                # TODO: exceptions to catch: domain doesn't exist, list can't be created, connection to server fails.
                return
        else:
            # VIESTIM: If a list with this name exists (it shouldn't since it's checked before allowing list creation,
            #  but technically it could if someone can grab the name during list creation process), then what do we do?
            return

    @staticmethod
    def check_name_rules(name_candidate: str) -> Tuple[bool, str]:
        """Check if name candidate complies with naming rules.

        :param name_candidate: What name we are checking.
        :return: Return True if name passes all rule checks. Otherwise return False. Also return an explanation string.
        """
        # Be careful when checking regex rules. Some rules allow a pattern to exist, while prohibiting others. Some
        # rules prohibit something, but allow other things to exist. If the explanation for a rule is different than
        # the regex, the explanation is more likely to be correct.

        # Name is within length boundaries.
        lower_bound = 5
        upper_bound = 36
        if not (lower_bound < len(name_candidate) < upper_bound):
            return False, "Name is not within length boundaries. Name has to be at least {0} and at most {1} " \
                          "characters long".format(lower_bound, upper_bound)

        # Name has to start with a lowercase letter.
        start_with_lowercase = re.compile(r"^[a-z]")
        if start_with_lowercase.search(name_candidate) is None:
            return False, "Name has to start with a lowercase letter."

        # Name cannot have multiple dots in sequence.
        no_sequential_dots = re.compile(r"\.\.+")
        if no_sequential_dots.search(name_candidate) is not None:
            return False, "Name cannot have sequential dots"

        # Name cannot end in a dot
        if name_candidate.endswith("."):
            return False, "Name cannot end in a dot."

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
            return False, "Name contains forbidden characters."

        # Name has to include at least one digit.
        required_digit = re.compile(r"\d")
        if required_digit.search(name_candidate) is None:
            return False, "Name has to include at least one digit."

        return True, "Name meets all the naming rules."


@dataclass
class EmailList:
    """Class to aid with email list specific functionality, such as attribute checking and changes.

    This class is designed to be used when an email list is expected to exits. Think operations like adding
    an email to an existing list etc. For operations other than that, use EmailListManager.
    """

    # VIESTIM: Would it be polite to return something as an indication how the operation went?

    @staticmethod
    def set_archive_type(listname: str, archive_status: bool) -> None:
        """
        Set list archiving on or off.

        :param listname: The email list
        :param archive_status: Models if this list is archived on Mailman's end. If True, all possible archivers are
         used. If False, no archivers are used.
        :return:
        """
        if _client is None:
            return
        try:
            mail_list = _client.get_list(listname)
            list_archivers = mail_list.archivers
            # VIESTIM: We set all archivers on or off here. Would we have any reason to be pick and choose possible
            #  archivers? Technicallyl the only one functional right now is hyper-kitty.
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
    def delete_email(listname: str, email: str) -> str:
        """
        Destructive email unsubscribtion. After this function has performed, the email is no longer on the list. If
        you intended to perform a soft removal, use other function for a "soft" deletion.

        :param listname: The list where the email is being removed.
        :param email: The email being removed.
        :return: A string informing operation success.
        """
        if _client is None:
            return "There is no connection to Mailman server. No deletion can be attempted."
        mlist: Optional[MailingList]
        try:
            # This might raise HTTPError
            mlist = _client.get_list(fqdn_listname=listname)
            # This might raise ValueError
            mlist.unsubscribe(email=email)
            return "{0} has been removed from {1}".format(email, listname)
        except HTTPError:
            return "List {0} is not found or connection to list program was severed.".format(listname)
        except ValueError:
            return "Address {0} doesn't exist on {1}. No removal performed.".format(email, listname)

    @staticmethod
    def add_email(listname: str, email: str, real_name: str, email_owner_pre_confirmation: bool) -> None:
        """
        Add email to a list.

        :param listname: List where a new email address is being added.
        :param email: The email address being added to a list.
        :param real_name: The real name associated with email.
        :param email_owner_pre_confirmation: If True, then the user *doesn't* have to confirm their joining. If False,
        then a confirmation email is sent to the email address before email is added to the list.
        :return:
        """
        if _client is None:
            return
        try:
            mlist: MailingList = _client.get_list(listname)
            # Viestim:
            #  We use pre_verify flag, because we assume email adder knows the address they are adding in. Otherwise
            #  they have to verify themselves for Mailman. Note that this is different from confirming to join a list.
            #  We use pre_approved flag, because we assume that who adds an email to a list also wants that email onto
            #  a list. Otherwise they would have to afterwards manually moderate their subscription request.
            mlist.subscribe(email,
                            pre_verified=True,
                            pre_approved=True,
                            pre_confirmed=email_owner_pre_confirmation,
                            display_name=real_name)
            return
        except HTTPError:
            # TODO: handle exceptions. Possible exceptions are list doesn't exits and email is already in the list.
            return

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
    def change_user_delivery_status(list_name: str, member_email: str, option: str, status: str) -> None:
        """ Change user's send or delivery status on an email list.

        :param list_name: List where we are changing delivery options.
        :param member_email: Which member's email's delivery status we are changing.
        :param option: Option can be 'delivery' or 'send'.
        :param status: A value that determines if option is 'enabled' or 'disabled'.
        :return:
        """
        # VIESTIM: We might want to change option and status parametrs from string to something like enum to rid
        #  ourselves from errors generated with typos.
        delivery = "delivery_status"
        if _client is None:
            return
        try:
            email_list = _client.get_list(list_name)
            member = email_list.get_member(member_email)
            member_preferences = member.preferences
            if option == delivery:
                # Viestim: This is just an idea how to go about changing user's delivery options. There exists
                #  frustratingly little documentation about this kind of thing, so this might not work. The idea
                #  originates from
                #  https://docs.mailman3.org/projects/mailman/en/latest/src/mailman/handlers/docs/owner-recips.html
                #  More information also at
                #  https://gitlab.com/mailman/mailman/-/blob/master/src/mailman/interfaces/member.py
                #
                # Change user's delivery off by setting "delivery_status" preference option to value "by_user"
                # or on by setting "delivery_status" preference option to "enabled".
                if status == "disabled":
                    member_preferences[delivery] = "by_user"
                if status == "enabled":
                    member_preferences[delivery] = "enabled"
                # Saving is required for changes to take effect.
                member_preferences.save()
            if option == "send":
                if status == "disabled":
                    member.moderation_action = "discard"
                if status == "enabled":
                    member.moderation_action = "accept"
                member.save()
            return
        except HTTPError:
            # TODO: Proper error handling.
            return

    @staticmethod
    def get_member_delivery_status(list_name: str, member_email: str) -> str:
        """
        Get member's delivery status.

        :param list_name: The list we are interested in.
        :param member_email: The member who's delivery status is being checked.
        :return: A string 'enabled' or 'disabled'.
        """
        if _client is None:
            return "No Mailman configuration."
        delivery_status = "delivery_status"
        try:
            email_list = _client.get_list(list_name)
            member = email_list.get_member(member_email)
            member_preferences = member.preferences
            if member_preferences[delivery_status] == "enabled":
                return "enabled"
            if member_preferences[delivery_status] == "by_user":
                # TODO: Do we need to check for other status?
                return "disabled"
            return "unknown"
        except HTTPError:
            # TODO: Error handling.
            return "Error on the connection or bad list name or member name"

    @staticmethod
    def get_member_send_status(list_name: str, member_email: str) -> str:
        if _client is None:
            return "No Mailman configuration"
        try:
            email_list = _client.get_list(list_name)
            member = email_list.get_member(member_email)
            if member.moderation_action == "accept":
                return "enabled"
            if member.moderation_action in ["discard", "reject", "hold"]:
                return "disabled"
            # TODO: Probably needs a better return value.
            return "unknown"
        except HTTPError:
            return "No connection to server, list doesn't exists or there member in questin is not on the list."

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
