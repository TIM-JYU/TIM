import re
from dataclasses import dataclass, field
from typing import Optional, List

from timApp.document.create_item import create_document
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.messaging.messagelist.messagelist_models import MessageListModel, Channel
from timApp.util.flask.requesthelper import RouteException
from timApp.util.utils import remove_path_special_chars


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
class Message:
    """A unified datastructure for messages TIM handles."""
    # Meta information about where this message belongs to and where it's from.
    message_list_name: str
    message_channel: Channel = field(metadata={'by_value': True})

    # Header information.
    sender: str
    reply_to: Optional[str]
    recipients: List[str]
    title: str

    # Message body.
    message_body: str

    # Email specific attributes.
    domain: Optional[str]


MESSAGE_LIST_DOC_PREFIX = "messagelists"
MESSAGE_LIST_ARCHIVE_PREFIX = "archives"


def archive_message(message_list: MessageListModel, message) -> None:
    """Archive a message for a message list."""
    # TODO: If there are multiple messages with same title, differentiate them.
    archive_title = message.title
    archive_path = f"{MESSAGE_LIST_ARCHIVE_PREFIX}/{remove_path_special_chars(archive_title)}"

    # Archive folder for message list.
    archive_folder = Folder.find_by_location(archive_path, message_list.name)

    archive_doc = create_document(archive_path, archive_title)

    # Set header information for archived message.
    archive_doc.document.add_text(f"Title: {message.title}")
    archive_doc.document.add_text(f"Sender: {message.sender}")
    archive_doc.document.add_text(f"Recipients: {message.recipients}")

    # Set message body for archived message.
    archive_doc.document.add_text(f"{message.message_body}")

    # From the archive folder, query all documents, sort them by created attribute. We do this to get the previously
    # newest archived message, before we create a archive document for newest message.
    all_archived_messages = []
    if archive_folder is not None:
        all_archived_messages = archive_folder.get_all_documents()
    else:
        # TODO: Set folder's owners to be message list's owners.
        manage_doc_block = Block.query.filter_by(id=message_list.manage_doc_id).one()
        owners = manage_doc_block.owners()
        Folder.create(archive_path, owner_groups=owners, title=f"{message_list.name}")

    if len(all_archived_messages) > 1:
        sorted_messages = sorted(all_archived_messages, key=lambda document: document.block.created, reverse=True)
        previous_doc = sorted_messages[1]

        # Set footer information for archived message. Footer information is not set for the very first message,
        # it get's it's link to next message when a second message is archived.

        # VIESTIM: Do we need other type of URL to previous_doc and archive_doc? Is url attribute enough?
        previous_doc_title = "Previous message"
        previous_doc_link = f"{previous_doc.url}"
        previous_message_link = f"[{previous_doc_title}]({previous_doc_link})"
        archive_doc.document.add_text(f"{previous_message_link}")

        next_doc_title = "Next message"
        next_doc_link = f"{archive_doc.url}"
        previous_doc.document.add_text(f"[{next_doc_title}]({next_doc_link})")

    # TODO: Set proper rights to the document. The message sender owns the document. Owners of the list get at least a
    #  view right. Other rights depend on the message list's archive policy.


def parse_mailman_message(original: dict, msg_list: MessageListModel) -> Message:
    """Modify an email message sent from Mailman to TIM's universal message format."""
    # VIESTIM: original should have fields specified in https://pypi.org/project/mail-parser/
    visible_recipients= []

    visible_recipients.extend(original["to"])
    visible_recipients.extend(original["cc"])
    # VIESTIM: How should we differentiate with cc and bcc in TIM's context? bcc recipients should still get messages
    #  intented for them.

    message = Message(
        message_list_name=msg_list.name,
        domain=msg_list.email_list_domain,
        message_channel=Channel.EMAIL_LIST,

        # Header information
        sender=original["from_"],
        reply_to=original["reply_to"],
        recipients=visible_recipients,
        title=original["subject"],

        # Message body
        message_body=original["body"],
    )
    return message
