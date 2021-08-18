import re
from dataclasses import dataclass, field
from datetime import datetime
from email.utils import parsedate_to_datetime
from enum import Enum
from typing import Optional, List, Dict, Tuple, Iterator, Any

from mailmanclient import MailingList, Client, Member
from mailmanclient.restbase.connection import Connection

from timApp.auth.accesshelper import has_manage_access, AccessDenied
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import create_document, apply_template
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.document import Document
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.validation import ItemValidationRule
from timApp.messaging.messagelist.emaillist import get_email_list_by_name, set_notify_owner_on_list_change, \
    set_email_list_unsubscription_policy, set_email_list_subject_prefix, set_email_list_only_text, \
    set_email_list_allow_nonmember, set_email_list_allow_attachments, set_email_list_default_reply_type, \
    add_email, get_email_list_member, remove_email_list_membership, set_email_list_member_send_status, \
    set_email_list_member_delivery_status, set_email_list_description, set_email_list_info
from timApp.messaging.messagelist.listinfo import ArchiveType, ListInfo, ReplyToListChanges
from timApp.messaging.messagelist.messagelist_models import MessageListModel, Channel, MessageListTimMember, \
    MessageListExternalMember, MessageListMember
from timApp.timdb.sqa import db
from timApp.user.groups import verify_groupadmin
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException
from timApp.util.logger import log_warning
from timApp.util.utils import remove_path_special_chars, get_current_time


def verify_can_create_lists() -> None:
    curr_user = get_current_user_object()
    res = verify_groupadmin(False, curr_user) or curr_user.is_sisu_teacher
    if not res:
        raise AccessDenied("This action requires permission to create message lists")


def verify_messagelist_name_requirements(name_candidate: str) -> None:
    """Checks name requirements specific for email list.

    If at any point a name requirement check fails, then an exception is raised an carried to the client. If all
    name requirements are met, then succeed silently.

    :param name_candidate: Name to check against naming rules.
    """
    # There might become a time when we also check here if name is some message list specific reserved name. We
    # haven't got a source of those reserved names, not including names that already exists, so no check at this time.
    verify_name_rules(name_candidate)
    verify_name_availability(name_candidate)


def verify_name_availability(name_candidate: str) -> None:
    """Check if a message list with a given name already exists.

    :param name_candidate: The name to be checked if it already exists.
    """
    if MessageListModel.name_exists(name_candidate):
        raise RouteException(f"Message list with name {name_candidate} already exists.")


# Regular expression patters used for name rule verification. They are kept here, so they are not re-compiled at
# every name rule verification. The explanation of the rules is at their usage in verify_name_rules function.
START_WITH_LOWERCASE_PATTER = re.compile(r"^[a-z]")
SEQUENTIAL_DOTS_PATTERN = re.compile(r"\.\.+")
# A Name cannot have allowed characters. This set of characters is an import from Korppi's character
# limitations for email list names, and can probably be expanded in the future if desired.
#     lowercase letters a - z
#     digits 0 - 9
#     dot '.'
#     hyphen '-'
#     underscore '_'
# The pattern is a negation of the actual rules.
PROHIBITED_CHARACTERS_PATTERN = re.compile(r"[^a-z0-9.\-_]")
REQUIRED_DIGIT_PATTERN = re.compile(r"\d")


class NameRequirements(Enum):
    NAME_LENGTH_BOUNDED = 0
    START_WITH_LOWERCASE = 1
    NO_SEQUENTIAL_DOTS = 2
    NO_TRAILING_DOTS = 3
    NO_FORBIDDEN_CHARS = 4
    MIN_ONE_DIGIT = 5


def check_name_rules(name_candidate: str) -> Iterator[NameRequirements]:
    """Check if name candidate complies with naming rules.

    :param name_candidate: What name we are checking against the rules.
    :return: A generator that returns violated name rules.
    """
    # Be careful when checking regex rules. Some rules allow a pattern to exist, while prohibiting others. Some
    # rules prohibit something, but allow other things to exist. If the explanation for a rule is different than
    # the regex, the explanation is more likely to be correct.

    # Name is within length boundaries.
    lower_bound = 5
    upper_bound = 36
    if not (lower_bound <= len(name_candidate) <= upper_bound):
        yield NameRequirements.NAME_LENGTH_BOUNDED

    # Name has to start with a lowercase letter.
    if not START_WITH_LOWERCASE_PATTER.search(name_candidate):
        yield NameRequirements.START_WITH_LOWERCASE

    # Name cannot have multiple dots in sequence.
    if SEQUENTIAL_DOTS_PATTERN.search(name_candidate):
        yield NameRequirements.NO_SEQUENTIAL_DOTS

    # Name cannot end in a dot.
    if name_candidate.endswith("."):
        yield NameRequirements.NO_TRAILING_DOTS

    if PROHIBITED_CHARACTERS_PATTERN.search(name_candidate):
        yield NameRequirements.NO_FORBIDDEN_CHARS

    # Name has to include at least one digit.
    if not REQUIRED_DIGIT_PATTERN.search(name_candidate):
        yield NameRequirements.MIN_ONE_DIGIT


def verify_name_rules(name_candidate: str) -> None:
    """Check if name candidate complies with naming rules.

    The function raises a RouteException if naming rule is violated. If this function doesn't raise an exception,
    then the name candidate follows naming rules.

    :param name_candidate: What name we are checking against the rules.
    """
    res = next(check_name_rules(name_candidate), None)
    if res:
        raise RouteException(res.name)


@dataclass
class EmailAndDisplayName:
    """Wrapper for parsed email messages containing sender/receiver email and display name."""
    email_address: str
    display_name: str

    def __repr__(self) -> str:
        """The representation of an email and display name for an email message is

        Jane Doe <jane.doe@domain.com>

        or just

        <john.doe@domain.com>

        if no name is associated with the email address.
        """
        if self.display_name:
            return f"{self.display_name} <{self.email_address}>"
        return f"<{self.email_address}>"


@dataclass
class BaseMessage:
    """A unified datastructure for messages TIM handles."""
    # Meta information about where this message belongs to and where its from. Mandatory values for all messages.
    message_list_name: str
    message_channel: Channel = field(metadata={'by_value': True})  # Where the message came from.

    # Header information. Mandatory values for all messages.
    sender: EmailAndDisplayName
    recipients: List[EmailAndDisplayName]
    subject: str

    # Message body. Mandatory value for all messages.
    message_body: str

    # Email specific attributes.
    domain: Optional[str] = None
    reply_to: Optional[EmailAndDisplayName] = None

    # Timestamp for the message is a mandatory value. If the message comes from an outside source, it should already
    # have a time stamp. The default value is mostly for messages that would be generated inside TIM. It can also be
    # set for messages which for some reason don't already have any form of timestamp present.
    timestamp: datetime = get_current_time()


# Path prefixes for documents and folders.
MESSAGE_LIST_DOC_PREFIX = "messagelists"
MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX = "archives"


def create_archive_doc_with_permission(archive_subject: str, archive_doc_path: str, message_list: MessageListModel,
                                       message: BaseMessage) -> DocEntry:
    """Create archive document with permissions matching the message list's archive policy.

    :param archive_subject: The subject of the archive document.
    :param archive_doc_path: The path where the archive document should be created.
    :param message_list: The message list where the message belongs.
    :param message: The message about to be archived.
    :return: The archive document.
    """
    # Gather owners of the archive document.
    message_owners: List[UserGroup] = []
    message_sender = User.get_by_email(message.sender.email_address)

    # List owners get a default ownership for the messages on a list. This covers the archive policy of SECRET.
    message_owners.extend(get_message_list_owners(message_list))

    # Who gets to see a message in the archives.
    message_viewers: List[UserGroup] = []

    # Gather permissions to the archive doc. The meanings of different archive settings are listed with ArchiveType
    # class.
    if message_list.archive_policy is ArchiveType.PUBLIC or ArchiveType.UNLISTED:
        message_viewers.append(UserGroup.get_anonymous_group())
        if message_sender:
            message_owners.append(message_sender.get_personal_group())
    elif message_list.archive_policy is ArchiveType.UNLISTED:
        message_viewers.append(UserGroup.get_logged_in_group())
        if message_sender:
            message_owners.append(message_sender.get_personal_group())
    elif message_list.archive_policy is ArchiveType.GROUPONLY:
        message_viewers = [m.user_group for m in message_list.get_tim_members()]
        if message_sender:
            message_owners.append(message_sender.get_personal_group())

    # If we don't provide at least one owner up front, then current user is set as owner. We don't want that,
    # because in this context that is the anonymous user, which raises an error in document creation.
    archive_doc = DocEntry.create(title=archive_subject, path=archive_doc_path, owner_group=message_owners[0])

    # Add the rest of the message owners.
    if len(message_owners) > 1:
        archive_doc.block.add_rights(message_owners[1:], AccessType.owner)

    # Add view rights.
    archive_doc.block.add_rights(message_viewers, AccessType.view)

    return archive_doc


def archive_message(message_list: MessageListModel, message: BaseMessage) -> None:
    """Archive a message for a message list.

    :param message_list: The message list where the archived message belongs.
    :param message: The message being archived.
    """
    # Archive policy of no archiving is a special case, where we abort immediately since these won't be archived at all.
    if message_list.archive_policy is ArchiveType.NONE:
        return

    archive_subject = f"{message.subject}"
    archive_folder_path = f"{MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX}/{remove_path_special_chars(message_list.name)}"
    archive_doc_path = remove_path_special_chars(f"{archive_folder_path}/{archive_subject}-"
                                                 f"{get_current_time().strftime('%Y-%m-%d %H:%M:%S')}")

    # From the archive folder, query all documents, sort them by created attribute. We do this to get the previously
    # newest archived message, before we create a archive document for newest message.
    # Archive folder for message list.
    archive_folder = Folder.find_by_path(archive_folder_path)
    all_archived_messages = []
    if archive_folder is not None:
        all_archived_messages = archive_folder.get_all_documents()
    else:
        owners = get_message_list_owners(message_list)
        Folder.create(archive_folder_path, owner_groups=owners, title=f"{message_list.name}")

    archive_doc = create_archive_doc_with_permission(archive_subject, archive_doc_path, message_list, message)

    # Set header information for archived message.
    archive_doc.document.add_text(f"""
#- {{ .mailheader}}\n
[{message.subject}]{{.mailtitle}}\\
Sender: {message.sender}\\
Recipients: {message.recipients}\\
Date: {message.timestamp.strftime('%Y-%m-%d %H:%M:%S')}
""")

    # Set message body for archived message.
    # TODO: Check message list's only_text flag.
    archive_doc.document.add_text(f"{message.message_body}")

    if not len(all_archived_messages):
        # If the message currently being archived is the very first one, it can't be linked to other messages.
        db.session.commit()
        return

    # Linking messages.

    sorted_messages = sorted(all_archived_messages, key=lambda document: document.block.created, reverse=True)
    # Get all_archived_messages before creating the archive document for the newest message, so the previous newest
    # message is at index 0.
    previous_doc = sorted_messages[0]

    if len(all_archived_messages) == 1:
        # Having only one message in the archive at first before "this newest" is a special case, because at that
        # point the first links to a next message has not been set. Setting link to a next message assumes that a
        # link to previous-previous message exits in the previous document.
        set_message_link_next(previous_doc.document, archive_doc.title, archive_doc.url,
                              archive_init_flag=True)
    else:
        set_message_link_next(previous_doc.document, archive_doc.title, archive_doc.url)
    set_message_link_previous(archive_doc.document, previous_doc.title, previous_doc.url)

    db.session.commit()


def set_message_link_next(doc: Document, link_text: str, url_next: str, archive_init_flag: bool = False) -> None:
    """Set links to a document from another document.

    This function sets a footer link to next archived document and a button to the header section with a link to next
    archived document.

    :param doc: The document where the link is appended.
    :param link_text: The text the link gets.
    :param url_next: The link to another document.
    :param archive_init_flag: A boolean flag, denoting if doc should be treated as the special case of being very first
    message in the archive when linking is started.
    """

    # Also add the directional button to the next document at the start of the header section.
    direct_button = f"[[>]{{.timButton}}]({url_next})"
    header = doc.get_paragraphs()[0]
    header_md = header.get_markdown()

    link_footer = f"[Next Message: {link_text}]({url_next})"
    if archive_init_flag:
        # Here we set the very first message's button and link to next. For some reason, the trailing whitespace is
        # needed for the directional button to appear on the document. The text for the directional button will be
        # there regardless, but it won't show on the document. So don't remove the trailing whitespace unless you
        # know that you get the link button to appear on the first archvie document some other way.
        button_first = f"{direct_button}\n\n{header_md}"
        header.set_markdown(button_first)
        header.save()

        footer_text = f"""
#- {{ .mailfooter}}\n
{link_footer}
"""
        doc.add_text(footer_text)
    else:
        # Find the index of character combination of ']{.ma'. That denotes the closing of the area of class
        # mailbrowsebuttons defined in the else part of this conditional statement. Inject the button to next
        # document there.
        limit = header_md.find("]{.ma")
        new_button_set = f"{header_md[0:limit]}\n{direct_button}{header_md[limit:]}"
        header.set_markdown(new_button_set)
        header.save()

        last_par = doc.get_paragraphs()[-1]
        last_par_md = last_par.get_markdown()
        # Add a \ and a line break after the previous link, then the new link.
        modified_footer = f"""{last_par_md}\\
{link_footer}"""
        last_par.set_markdown(modified_footer)
        last_par.save()


def set_message_link_previous(doc: Document, link_text: str, url_previous: str) -> None:
    """Set links to a document from another document.

    This function sets a footer link to previous archived document and a button to the header section with a link to
    revious archived document.

    :param doc: The document where the link is appended.
    :param link_text: The text the link gets.
    :param url_previous: The link to another document.
    """
    # Add footer for link to previous document.
    link_footer = f"""
#- {{ .mailfooter}}\n
[Previous message: {link_text}]({url_previous})
"""
    doc.add_text(link_footer)
    # Add a directional button at the start of the header section of document.
    direct_button = f"[[[<]{{.timButton}}]({url_previous})]{{.mailbrowsebuttons}}\n\n"
    header = doc.get_paragraphs()[0]
    header.insert_before_md(direct_button)
    header.save()


def parse_mailman_message(original: Dict, msg_list: MessageListModel) -> BaseMessage:
    """Modify an email message sent from Mailman to TIM's universal message format.

    :param original: An email message sent from Mailman.
    :param msg_list: The message list where original is meant to go.
    :return: A BaseMessage object corresponding the original email message.
    """
    # original message is of form specified in https://pypi.org/project/mail-parser/
    visible_recipients: List[EmailAndDisplayName] = []
    maybe_to_addresses = parse_mailman_message_address(original, "to")
    if maybe_to_addresses is not None:
        visible_recipients.extend(maybe_to_addresses)
    maybe_cc_addresses = parse_mailman_message_address(original, "cc")
    if maybe_cc_addresses is not None:
        visible_recipients.extend(maybe_cc_addresses)

    sender: Optional[EmailAndDisplayName] = None
    maybe_from_address = parse_mailman_message_address(original, "from")
    if maybe_from_address is not None:
        # Expect only one sender.
        sender = maybe_from_address[0]
    if sender is None:
        # If no sender is found on a message, we don't archive the message.
        raise RouteException("No sender found in the message.")

    message = BaseMessage(
        message_list_name=msg_list.name,
        domain=msg_list.email_list_domain,
        message_channel=Channel.EMAIL_LIST,

        # Header information
        sender=sender,
        recipients=visible_recipients,
        subject=original["subject"],

        # Message body
        message_body=original["body"],
    )

    # Try parsing the rest of email specific fields.
    if "reply_to" in original:
        message.reply_to = original["reply_to"]
    if "date" in original:
        try:
            # At first we except RFC5322 format Date header.
            message.timestamp = parsedate_to_datetime(original["date"])
        except (TypeError, ValueError):
            # Being here means that the date field is not in RFC5322 format. Testing has shown that ISO8601 format is
            # then a likely candidate format for Date header. Try parsing that format.
            try:
                message.timestamp = datetime.fromisoformat(original["date"])
            except ValueError:
                # Being here means that the date field was none of tried formats after all. We'll log the format the
                # date was in so that it can be fixed.
                log_warning(
                    f"Function parse_mailman_message has encountered a Date header format it cannot handle. The "
                    f"date is of format {original['date']}. Please handle this at earliest convenience.")
    return message


def parse_mailman_message_address(original: Dict, header: str) -> Optional[List[EmailAndDisplayName]]:
    """Parse (potentially existing) fields 'from' 'to', 'cc', or 'bcc' from a dict representing Mailman's email message.
    The fields are in lists, with individual list indicies being lists themselves of the form
        ['Display Name', 'email@domain.fi']

    :param original: Original message.
    :param header: One of "from", "to", "cc" or "bcc".
    :return: Return None if the header is not one of "from", "to", "cc" or "bcc". Otherwise return a list of
    EmailAndDisplayName objects.
    """

    if header not in ["from", "to", "cc", "bcc"]:
        return None

    email_name_pairs: List[EmailAndDisplayName] = []

    if header in original:
        for email_name_pair in original[header]:
            new_email_name_pair = EmailAndDisplayName(email_address=email_name_pair[1],
                                                      display_name=email_name_pair[0])
            email_name_pairs.append(new_email_name_pair)

    return email_name_pairs


def get_message_list_owners(mlist: MessageListModel) -> List[UserGroup]:
    """Get the owners of a message list.

    :param mlist: The message list we want to know the owners.
    :return: A list of owners, as their personal user group.
    """
    manage_doc_block = Block.query.filter_by(id=mlist.manage_doc_id).one()
    return manage_doc_block.owners


def create_management_doc(msg_list_model: MessageListModel, list_options: ListInfo) -> DocInfo:
    """Create management doc for a new message list.

    :param msg_list_model: The message list the management document is created for.
    :param list_options: Options for creating the management document.
    :return: Newly created management document.
    """

    doc = create_document(f'/{MESSAGE_LIST_DOC_PREFIX}/{remove_path_special_chars(list_options.name)}',
                          list_options.name,
                          validation_rule=ItemValidationRule(check_write_perm=False),
                          parent_owner=UserGroup.get_admin_group())

    apply_template(doc)
    s = doc.document.get_settings().get_dict().get('macros', {})
    s['messagelist'] = list_options.name
    doc.document.add_setting('macros', s)

    # Set the management doc for the message list.
    msg_list_model.manage_doc_id = doc.id

    return doc


def new_list(list_options: ListInfo) -> Tuple[DocInfo, MessageListModel]:
    """Adds a new message list into the database and creates the list's management doc.

    :param list_options: The list information for creating a new message list. Used to carry list's name and archive
    policy.
    :return: The management document of the message list.
    :return: The message list db model.
    """
    msg_list = MessageListModel(name=list_options.name, archive=list_options.archive)
    db.session.add(msg_list)
    doc_info = create_management_doc(msg_list, list_options)
    return doc_info, msg_list


def set_message_list_notify_owner_on_change(message_list: MessageListModel,
                                            notify_owners_on_list_change_flag: Optional[bool]) -> None:
    """Set the notify list owner on list change flag for a list, and update necessary channels with this information.

    If the message list has an email list as a message channel, this will set the equilavent flag on the email list.

    :param message_list: The message list where the flag is being set.
    :param notify_owners_on_list_change_flag: An optional boolean flag. If True, then changes on the message list sends
    notifications to list owners. If False, notifications won't be sent. If None, nothing is set.
    """
    if notify_owners_on_list_change_flag is None \
            or message_list.notify_owner_on_change == notify_owners_on_list_change_flag:
        return

    message_list.notify_owner_on_change = notify_owners_on_list_change_flag

    if message_list.email_list_domain:
        # Email lists have their own flag for notifying list owners for list changes.
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_notify_owner_on_list_change(email_list, message_list.notify_owner_on_change)


def set_message_list_member_can_unsubscribe(message_list: MessageListModel,
                                            can_unsubscribe_flag: Optional[bool]) -> None:
    """Set the list member's free unsubscription flag, and propagate that setting to channels that have own handling
    of unsubscription.

    If the message list has an email list as a message channel, this will set the equilavent flag on the email list.

    :param message_list: Message list where the flag is being set.
    :param can_unsubscribe_flag: An optional boolean flag. For True, the member can unsubscribe on their own. For False,
     then the member can't unsubscribe from the list on their own. If None, then the current value is kept.
    """
    if can_unsubscribe_flag is None or message_list.can_unsubscribe == can_unsubscribe_flag:
        return
    message_list.can_unsubscribe = can_unsubscribe_flag

    if message_list.email_list_domain:
        # Email list's have their own settings for unsubscription.
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_unsubscription_policy(email_list, can_unsubscribe_flag)


def set_message_list_subject_prefix(message_list: MessageListModel, subject_prefix: Optional[str]) -> None:
    """Set the message list's subject prefix.

    If the message list has an email list as a message list, then set the subject prefix there also.

    Sets one extra space automatically to offset prefix from the actual title.

    :param message_list: The message list where the subject prefix is being set.
    :param subject_prefix: The prefix set for messages that go through the list. If None, then the current value is
    kept.
    """
    if subject_prefix is None or message_list.subject_prefix == subject_prefix:
        return

    # Add an extra space, if there is none.
    if not subject_prefix.endswith(" "):
        subject_prefix = f"{subject_prefix} "

    message_list.subject_prefix = subject_prefix

    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_subject_prefix(email_list, subject_prefix)


def set_message_list_tim_users_can_join(message_list: MessageListModel, can_join_flag: Optional[bool]) -> None:
    """Set the flag controlling if TIM users can directly join this list.

    Because the behaviour that is controlled by the can_join_flag applies to TIM users, there is no message channel
    specific handling.

    :param message_list: Message list where the flag is being set.
    :param can_join_flag: An optional boolean flag. If True, then TIM users can directly join this list, no moderation
    needed. If False, then TIM users can't direclty join the message list. If None, the current value is kept.
    """
    if can_join_flag is None or message_list.tim_user_can_join == can_join_flag:
        return

    message_list.tim_user_can_join = can_join_flag


def set_message_list_default_send_right(message_list: MessageListModel,
                                        default_send_right_flag: Optional[bool]) -> None:
    """Set the default message list new member send right flag.

    :param message_list: The message list where the flag is set.
    :param default_send_right_flag: An optional boolean flag. For True, new members on the list get default send right.
    For False, new members don't get a send right. For None, the current value is kept.
    """
    if default_send_right_flag is None or message_list.default_send_right == default_send_right_flag:
        return
    message_list.default_send_right = default_send_right_flag


def set_message_list_default_delivery_right(message_list: MessageListModel,
                                            default_delivery_right_flag: Optional[bool]) -> None:
    """Set the message list new member default delivery right.

    :param message_list: The message list where the flag is set.
    :param default_delivery_right_flag: An optional boolean flag. For True, new members on the list get default delivery
    right. For False, new members don't automatically get a delivery right. For None, the current value is kept.
    """
    if default_delivery_right_flag is None or message_list.default_delivery_right == default_delivery_right_flag:
        return
    message_list.default_delivery_right = default_delivery_right_flag


def set_message_list_only_text(message_list: MessageListModel, only_text: Optional[bool]) -> None:
    """Set the flag controlling if message list is to accept text-only messages.

    :param message_list: The message list where the flag is to be set.
    :param only_text: An optional boolean flag. For True, the message list is set to text-only mode. For False, the
    message list accepts HTML-based messages. For None, the current value is kept.
    """
    if only_text is None or message_list.only_text == only_text:
        return
    message_list.only_text = only_text

    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_only_text(email_list, only_text)


def set_message_list_non_member_message_pass(message_list: MessageListModel,
                                             non_member_message_pass_flag: Optional[bool]) -> None:
    """Set message list's non member message pass flag.

    :param message_list: The message list where the flag is set.
    :param non_member_message_pass_flag: An optional boolean flag. For True, sources outside the list can send messages
    to this list. If False, messages form sources outside the list will be hold for moderation. For None, the current
    value is kept.
    """
    if non_member_message_pass_flag is None or message_list.non_member_message_pass == non_member_message_pass_flag:
        return
    message_list.non_member_message_pass = non_member_message_pass_flag
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_allow_nonmember(email_list, non_member_message_pass_flag)


def set_message_list_allow_attachments(message_list: MessageListModel, allow_attachments_flag: Optional[bool]) -> None:
    """Set the flag controlling if a message list accepts messages with attachments.

    :param message_list: The message list where the flag is to be set.
    :param allow_attachments_flag: An optional boolean flag. For True, the list will allow a pre-determined set of
    attachments. For False, no attachments are allowed. For None, the current value is kept.
    """
    if allow_attachments_flag is None or message_list.allow_attachments == allow_attachments_flag:
        return

    message_list.allow_attachments = allow_attachments_flag
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_allow_attachments(email_list, allow_attachments_flag)


def set_message_list_default_reply_type(message_list: MessageListModel,
                                        default_reply_type: Optional[ReplyToListChanges]) -> None:
    """Set a value controlling how replies to a message list are steered.

    The reply type is analogous to email lists' operation of "Reply-To munging". Reply-To munging is a process where
    messages sent to list may be subject to having their Reply-To header changed from what the sender of the message
    initially used. This is mainly used (and sometimes abused) to steer conversation from announce-only lists (which
    don't accept posts from anyone except few select individuals) to separate discussion lists.

    :param message_list: The message list where the value is to be set.
    :param default_reply_type: An optional enumeration. For value NOCHANGES the user is completely left the control
    how to respond to messages sent from the list. For value ADDLIST the replies will be primarily steered towards
    the message list. For None, the current value is kept.
    """
    if default_reply_type is None or message_list.default_reply_type == default_reply_type:
        return

    message_list.default_reply_type = default_reply_type
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_default_reply_type(email_list, default_reply_type)


def add_new_message_list_group(msg_list: MessageListModel, ug: UserGroup,
                               send_right: bool, delivery_right: bool, em_list: Optional[MailingList]) -> None:
    """Add new (user) group to a message list.

    For groups, checks that the adder has at least manage rights to group's admin doc.

    Performs a duplicate check for memberships. A duplicate member will not be added again to the list. The process
    of re-activating a removed member of a list is different. For re-activating an already existing member,
    use set_message_list_member_removed_status function.

    This is a direct add, meaning member's membership_verified attribute is set in this function. Use other means to
    invite members.

    :param msg_list: The message list where the group will be added.
    :param ug: The user group being added to a message list.
    :param send_right: Send right for user groups members, that will be added to the message list individually.
    :param delivery_right: Delivery right for user groups members, that will be added to the message list individually.
    :param em_list: An optional email list. If given, then all the members of the user group will also be subscribed to
    the email list.
    """
    # Check right to a group. Right checking is not required for personal groups, only user groups.
    if not ug.is_personal_group and not has_manage_access(ug.admin_doc):
        return

    # Check for membership duplicates.
    member = msg_list.find_member(username=ug.name, email=None)
    if member and not member.membership_ended:
        return
    # Add the user group as a member to the message list.
    new_group_member = MessageListTimMember(message_list_id=msg_list.id, group_id=ug.id,
                                            delivery_right=delivery_right, send_right=send_right,
                                            membership_verified=get_current_time())
    db.session.add(new_group_member)

    # Add group's individual members to message channels.
    if em_list is not None:
        for user in ug.users:
            # TODO: Search for a set of emails and a primary email here when users' additional emails are implemented.
            user_email = user.email  # In the future, we can search for a set of emails and a primary email here.
            add_email(em_list, user_email, email_owner_pre_confirmation=True, real_name=user.real_name,
                      send_right=send_right, delivery_right=delivery_right)


def add_message_list_external_email_member(msg_list: MessageListModel, external_email: str,
                                           send_right: bool, delivery_right: bool, em_list: MailingList,
                                           display_name: Optional[str]) -> None:
    """Add external member to a message list. External members at this moment only support external members to email
     lists.

    :param msg_list: Message list where the member is to be added.
    :param external_email: The email address of an external member to be added to the message list.
    :param send_right: The send right to the list by the new member.
    :param delivery_right: The delivery right to the list by the new member.
    :param em_list: The email list where this external member will be also added, because at this time external members
    only make sense for an email list.
    :param display_name: Optional name associated with the external member.
    """
    # Check for duplicate members.
    if msg_list.find_member(username=None, email=external_email):
        return

    new_member = MessageListExternalMember(email_address=external_email, display_name=display_name,
                                           delivery_right=delivery_right, send_right=send_right,
                                           message_list_id=msg_list.id)
    db.session.add(new_member)
    add_email(em_list, external_email, email_owner_pre_confirmation=True, real_name=display_name,
              send_right=send_right, delivery_right=delivery_right)


def sync_message_list_on_add(user: User, new_group: UserGroup) -> None:
    """On adding a user to a new group, sync the user to user group's message lists.

    :param user: The user that was added to the new_group.
    :param new_group: The new group that the user was added to.
    """
    # TODO: This might become a bottle neck, as adding to group is often done in a loop and every sync is a potential
    #  call to different message channels (now just Mailman). In order to rid ourselves of that, we might need to
    #  revamp the syncing. A solution might be a call to (Mailman's) server (sidelining mailmanclient-library) with a
    #  batch of users we want to add with necessary information, and then let the server handle adding in a loop
    #  locally.

    # Get all the message lists for the user group.
    for group_tim_member in new_group.messagelist_membership:
        group_message_list: MessageListModel = group_tim_member.message_list
        # Propagate the adding on message list's message channels.
        if group_message_list.email_list_domain:
            email_list = get_email_list_by_name(group_message_list.name, group_message_list.email_list_domain)
            add_email(email_list, user.email, True, user.real_name,
                      group_tim_member.member.send_right, group_tim_member.member.delivery_right)


def sync_message_list_on_expire(user: User, old_group: UserGroup) -> None:
    """On removing a user from a user group, remove the user from all the message lists that watch the group.

    :param user: The user who was removed from the user group.
    :param old_group: The group where the user was removed from.
    """
    # TODO: This might become a bottle neck, as removing from group is often done in a loop and every sync is a
    #  potential call to different message channels (now just Mailman). In order to rid ourselves of that,
    #  we might need to revamp the syncing. A solution might be a call to (Mailman's) server (sidelining
    #  mailmanclient-library) with a batch of users we want to add with necessary information, and then let the
    #  server handle removing in a loop locally.

    # Get all the message lists for the user group.
    for group_tim_member in old_group.messagelist_membership:
        group_message_list: MessageListModel = group_tim_member.message_list
        # Propagate the deletion on message list's message channels.
        if group_message_list.email_list_domain:
            email_list = get_email_list_by_name(group_message_list.name, group_message_list.email_list_domain)
            email_list_member = get_email_list_member(email_list, user.email)
            remove_email_list_membership(email_list_member)


def set_message_list_member_removed_status(member: MessageListMember,
                                           removed: Optional[datetime], email_list: Optional[MailingList]) -> None:
    """Set the message list member's membership removed status.

    :param member: The member who's membership status is being set.
    :param removed: Member's date of removal from the message list. If None, then the member is an active member on the
    list.
    :param email_list: An email list belonging to the message list. If None, the message list does not have an email
    list.
    """
    if (member.membership_ended is None and removed is None) or (member.membership_ended and removed):
        return

    member.remove(removed)
    # Remove members from email list or return them there.
    if email_list:
        if member.is_group():
            ug = member.tim_member.user_group
            ug_members = ug.users
            for ug_member in ug_members:
                mlist_member = get_email_list_member(email_list, ug_member.email)
                if removed:
                    remove_email_list_membership(mlist_member)
                else:
                    # Re-set the member's send and delivery rights on the email list.
                    set_email_list_member_send_status(mlist_member, member.send_right)
                    set_email_list_member_delivery_status(mlist_member, member.delivery_right)
        elif member.is_personal_user():
            # Make changes to member's status on the email list.
            mlist_member = get_email_list_member(email_list, member.get_email())
            # If there is an email list and the member is removed, do a soft removal on the email list.
            if removed:
                remove_email_list_membership(mlist_member)
            else:
                # Re-set the member's send and delivery rights on the email list.
                set_email_list_member_send_status(mlist_member, member.send_right)
                set_email_list_member_delivery_status(mlist_member, member.delivery_right)


def set_member_send_delivery(member: MessageListMember, send: bool, delivery: bool,
                             email_list: Optional[MailingList] = None) -> None:
    """Set message list member's send and delivery rights.

    :param member: Member who's rights are being set.
    :param send: Member's new send right.
    :param delivery: Member's new delivery right.
    :param email_list: If the message list has email list as one of its message channels, set the send and delivery
     rights there also.
    :return: None.
    """
    # Send right
    if member.send_right != send:
        member.send_right = send
        if email_list:
            if member.is_personal_user():
                mlist_member = get_email_list_member(email_list, member.get_email())
                set_email_list_member_send_status(mlist_member, send)
            elif member.is_group():
                # For group, set the delivery status for its members on the email list.
                ug = member.tim_member.user_group
                ug_members = ug.users  # ug.current_memberships
                for ug_member in ug_members:
                    # user = ug_member.personal_user
                    email_list_member = get_email_list_member(email_list, ug_member.email)
                    set_email_list_member_send_status(email_list_member, send)

    # Delivery right.
    if member.delivery_right != delivery:
        member.delivery_right = delivery
        if email_list:
            # If message list has an email list associated with it, set delivery rights there.
            if member.is_personal_user():
                mlist_member = get_email_list_member(email_list, member.get_email())
                set_email_list_member_delivery_status(mlist_member, delivery)
            elif member.is_group():
                # For group, set the delivery status for its members on the email list.
                ug = member.tim_member.user_group
                ug_members = ug.users  # ug.current_memberships
                for ug_member in ug_members:
                    # user = ug_member.personal_user
                    email_list_member = get_email_list_member(email_list, ug_member.email)
                    set_email_list_member_delivery_status(email_list_member, delivery)


def set_message_list_description(message_list: MessageListModel, description: Optional[str]) -> None:
    """Set a (short) description to a message list and its associated message channels.

    :param message_list: The message list where the description is set.
    :param description: The new description. If None, keep the current value.
    """
    if description is None or message_list.description == description:
        return
    message_list.description = description
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_description(email_list, description)


def set_message_list_info(message_list: MessageListModel, info: Optional[str]) -> None:
    """Set a long description (called 'info' on Mailman) to a message list and its associated message channels.

    :param message_list: The message list where the (long) description is set.
    :param info: The new long description. If None, keep the current value.
    """
    if info is None or message_list.info == info:
        return
    message_list.info = info
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_info(email_list, info)


# Modified version of https://gitlab.com/mailman/mailmanclient/-/blob/master/src/mailmanclient/restobjects/user.py#L60
# to find subscriptions for only the given address
def find_members_for_address(client: Client, address: str) -> List[Member]:
    # Internal mailmanclient member used for authenticated REST calls
    # noinspection PyProtectedMember
    con: Connection = client._connection
    content: Dict[str, Any]
    _, content = con.call('members/find', data={'subscriber': address})
    try:
        return [Member(con, entry['self_link'], entry) for entry in content['entries']]
    except KeyError as e:
        print(e)
        pass
    return []
