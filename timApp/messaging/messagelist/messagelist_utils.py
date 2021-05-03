import re
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, List, Dict

from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.messaging.messagelist.listoptions import ArchiveType
from timApp.messaging.messagelist.messagelist_models import MessageListModel, Channel
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException
from timApp.util.utils import remove_path_special_chars, get_current_time


def check_messagelist_name_requirements(name_candidate: str) -> None:
    """Checks name requirements specific for email list.

    If at any point a name requirement check fails, then an exception is raised an carried to the client. If all
    name requirements are met, then succeed silently.

    :param name_candidate: Name to check against naming rules.
    """
    # Check name against name rules. These rules should also be checked client-side.
    check_name_rules(name_candidate)

    # Check name is available.
    check_name_availability(name_candidate)

    # There might become a time when we also check here if name is some message list specific reserved name. We
    # haven't got a source of those reserved names, not including names that already exists, so no check at this time.

    # If we are here, name can be used by the user.
    return


def check_name_availability(name: str) -> None:
    msg_list_exists = MessageListModel.name_exists(name)
    if msg_list_exists:
        raise RouteException(f"Message list with name {name} already exists.")
    return


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
    return


@dataclass
class EmailAndDisplayName:
    email_address: str
    display_name: str

    def __repr__(self) -> str:
        if self.display_name:
            return f"{self.display_name} <{self.email_address}>"
        return f"<{self.email_address}>"


@dataclass
class MessageTIMversalis:
    """A unified datastructure for messages TIM handles."""
    # Meta information about where this message belongs to and where it's from. Mandatory values for all messages.
    message_list_name: str
    message_channel: Channel = field(metadata={'by_value': True})  # Where the message came from.

    # Header information. Mandatory values for all messages.
    sender: EmailAndDisplayName
    recipients: List[EmailAndDisplayName]
    title: str

    # Message body. Mandatory value for all messages.
    message_body: str

    # Email specific attributes.
    domain: Optional[str] = None
    reply_to: Optional[EmailAndDisplayName] = None

    timestamp: datetime = get_current_time()


MESSAGE_LIST_DOC_PREFIX = "messagelists"
MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX = "archives"


def create_archive_doc_with_permission(archive_title: str, archive_doc_path: str, message_list: MessageListModel,
                                       message: MessageTIMversalis):
    # Gather owners of the archive document.
    # TODO: Create new document, setting the owner as either the person sending the message or message list's owner
    message_owners: List[UserGroup] = []
    message_owner = User.get_by_email(message.sender.email_address)
    if message_owner:
        message_owners.append(message_owner.get_personal_group())
    message_owners.extend(get_message_list_owners(message_list))

    # Add create archive document and add owners for the document.
    # VIESTIM: If we don't provide at least one owner up front, then current user is set as owner. We don't want
    #  that, because in this context that is the anonymous user, and that raises an error.
    archive_doc = DocEntry.create(title=archive_title, path=archive_doc_path, owner_group=message_owners[0])

    if len(message_owners) > 1:
        # Add the rest of the message owners.
        archive_doc.block.add_rights(message_owners[1:], AccessType.owner)

    if message_list.archive_policy is ArchiveType.PUBLIC:
        pass
    elif message_list.archive_policy is ArchiveType.UNLISTED:
        pass
    elif message_list.archive_policy is ArchiveType.GROUPONLY:
        pass
    elif message_list.archive_policy is ArchiveType.SECRET:
        pass

    return archive_doc


def archive_message(message_list: MessageListModel, message: MessageTIMversalis) -> None:
    """Archive a message for a message list."""
    # Archive policy of no archiving is a special case, where we abort immediately since these won't be archived at all.
    if message_list.archive_policy is ArchiveType.NONE:
        # VIESTIM: Do we need an exception here? Is it enough to just silently return?
        return

    archive_title = f"{message.title}-{get_current_time()}"
    archive_folder_path = f"{MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX}/{remove_path_special_chars(message_list.name)}"
    archive_doc_path = f"{archive_folder_path}/{remove_path_special_chars(archive_title)}"

    # From the archive folder, query all documents, sort them by created attribute. We do this to get the previously
    # newest archived message, before we create a archive document for newest message.
    # Archive folder for message list.
    archive_folder = Folder.find_by_path(archive_folder_path)
    all_archived_messages = []
    if archive_folder is not None:
        all_archived_messages = archive_folder.get_all_documents()
    else:
        # TODO: Set folder's owners to be message list's owners.
        # FIXME USE BETTER METHOD HERE
        manage_doc_block = Block.query.filter_by(id=message_list.manage_doc_id).one()
        owners = manage_doc_block.owners
        Folder.create(archive_folder_path, owner_groups=owners, title=f"{message_list.name}")

    archive_doc = create_archive_doc_with_permission(archive_title, archive_doc_path, message_list, message)

    # Set header information for archived message.
    archive_doc.document.add_text(f"Title: {message.title}")
    archive_doc.document.add_text(f"Sender: {message.sender}")
    archive_doc.document.add_text(f"Recipients: {message.recipients}")

    # Set message body for archived message.
    archive_doc.document.add_text("Message body:")
    archive_doc.document.add_text(f"{message.message_body}")

    # If there is only one message, we don't need to add links to any other messages.
    if len(all_archived_messages):
        # TODO: Extract linkings to their own functions.
        sorted_messages = sorted(all_archived_messages, key=lambda document: document.block.created, reverse=True)
        previous_doc = sorted_messages[0]

        # Set the "Next message" link for the previous newest message.
        next_doc_title = f"Next message: {archive_title}"
        next_doc_link = f"{archive_doc.url}"
        next_message_link = f"[{next_doc_title}]({next_doc_link})"
        previous_doc.document.add_text(next_message_link)

        # Set the "Previous message" link for the newest message.
        previous_doc_title = f"Previous message: {previous_doc.title}"
        previous_doc_link = f"{previous_doc.url}"
        previous_message_link = f"[{previous_doc_title}]({previous_doc_link})"
        archive_doc.document.add_text(f"{previous_message_link}")

    # TODO: Set proper rights to the document. The message sender owns the document. Owners of the list get at least a
    #  view right. Other rights depend on the message list's archive policy.
    db.session.commit()
    return


def parse_mailman_message(original: Dict, msg_list: MessageListModel) -> MessageTIMversalis:
    """Modify an email message sent from Mailman to TIM's universal message format."""
    # VIESTIM: original message is of form specified in https://pypi.org/project/mail-parser/
    # TODO: Get 'content-type' field, e.g. 'text/plain; charset="UTF-8"'
    # TODO: Get 'date' field, e.g. '2021-05-01T19:09:07'
    # VIESTIM: Get 'message-id-hash' field (maybe to check for duplicate messages), e.g.
    #  'H5IULFLU3PXSUPCBEXZ5IKTHX4SMCFHJ'
    visible_recipients: List[EmailAndDisplayName] = []
    maybe_to_addresses = parse_mailman_message_address(original, "to")
    if maybe_to_addresses is not None:
        visible_recipients.extend(maybe_to_addresses)
    maybe_cc_addresses = parse_mailman_message_address(original, "cc")
    if maybe_cc_addresses is not None:
        visible_recipients.extend(maybe_cc_addresses)

    # VIESTIM: How should we differentiate with cc and bcc in TIM's context? bcc recipients should still get messages
    #  intented for them.
    sender: Optional[EmailAndDisplayName] = None
    maybe_from_address = parse_mailman_message_address(original, "from")
    if maybe_from_address is not None:
        sender = maybe_from_address[0]
    if sender is None:
        # VIESTIM: Should there be a reasonable exception that messages always have to have a sender (and only one
        #  sender), and if not then they are dropped? What good can be a (email) message if there is no sender field?
        raise RouteException("No sender found in the message.")

    message = MessageTIMversalis(
        message_list_name=msg_list.name,
        domain=msg_list.email_list_domain,
        message_channel=Channel.EMAIL_LIST,

        # Header information
        sender=sender,  # VIESTIM: Message should only have one sender?
        recipients=visible_recipients,
        title=original["subject"],

        # Message body
        message_body=original["body"],
    )

    # Try parsing the rest of email spesific fields.
    if "reply_to" in original:
        message.reply_to = original["reply_to"]

    return message


def parse_mailman_message_address(original: Dict, header: str) -> Optional[List[EmailAndDisplayName]]:
    """Parse (potentially existing) fields 'from' 'to', 'cc', or 'bcc' from a dict representing Mailman's email message.
    The fields are in lists, with individual list indicies being lists themselves of the form
        ['Display Name', 'email@domain.fi']

    :param original: Original message.
    :param header: One of "from", "to", "cc" or "bcc".
    """

    if header not in ["from", "to", "cc", "bcc"]:
        return None

    list_of_emails: List[EmailAndDisplayName] = []

    if header in original:
        for email_name_pair in original[header]:
            new_email_name_pair = EmailAndDisplayName(email_address=email_name_pair[1],
                                                      display_name=email_name_pair[0])
            list_of_emails.append(new_email_name_pair)

    return list_of_emails


def get_message_list_owners(mlist: MessageListModel) -> List[UserGroup]:
    manage_doc_block = Block.query.filter_by(id=mlist.manage_doc_id).one()
    return manage_doc_block.owners
