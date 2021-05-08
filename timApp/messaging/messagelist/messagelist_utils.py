import re
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, List, Dict

from timApp.auth.accesstype import AccessType
from timApp.document.create_item import create_document
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.document import Document
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.messaging.messagelist.emaillist import get_email_list_by_name, set_notify_owner_on_list_change, \
    set_email_list_unsubscription_policy, set_email_list_subject_prefix, set_email_list_only_text, \
    set_email_list_non_member_message_pass, set_email_list_allow_attachments, set_email_list_default_reply_type
from timApp.messaging.messagelist.listoptions import ArchiveType, ListOptions, ReplyToListChanges
from timApp.messaging.messagelist.messagelist_models import MessageListModel, Channel, MessageListTimMember
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

    # VIESTIM: Would a response depth field be usefull? It was stated, that multiple Re: and Vs: prefixes on subjects
    #  is annoying and these should be discarded, but if we wish to do some type of inferance in the future about
    #  what a message is responding at, it might need this information.


MESSAGE_LIST_DOC_PREFIX = "messagelists"
MESSAGE_LIST_ARCHIVE_FOLDER_PREFIX = "archives"


def message_list_tim_members_as_user_groups(tim_members: List['MessageListTimMember']) -> List[UserGroup]:
    user_groups = []
    for member in tim_members:
        user_groups.append(member.user_group)
    return user_groups


def create_archive_doc_with_permission(archive_title: str, archive_doc_path: str, message_list: MessageListModel,
                                       message: MessageTIMversalis) -> DocEntry:
    """Create archive document with permissions matching the message list's archive type.

    :param archive_title: The title of the archive document.
    :param archive_doc_path: The path where the archive document should be created.
    :param message_list: The message list where the message belongs.
    :param message: The message about to be archived.
    :return: The archive document.
    """
    # Gather owners of the archive document.
    # TODO: Create new document, setting the owner as either the person sending the message or message list's owner
    message_owners: List[UserGroup] = []
    message_sender = User.get_by_email(message.sender.email_address)

    # List owners get a default ownership for the messages on a list.
    message_owners.extend(get_message_list_owners(message_list))

    # Who gets to see a message in the archives.
    message_viewers: List[UserGroup] = []

    # Gather permissions to the archive doc. The meanings of different archive settings are listed with ArchiveType
    # class.
    if message_list.archive_policy is ArchiveType.PUBLIC or ArchiveType.UNLISTED:
        # Unlisted and public archiving only differs in whether or not the archive folder is in a special place
        # where it can be found more easily. The folder is linked/aliased elsewhere and is not a concer in archiving.
        message_viewers.append(UserGroup.get_anonymous_group())
        if message_sender:
            message_owners.append(message_sender.get_personal_group())
    elif message_list.archive_policy is ArchiveType.GROUPONLY:
        message_viewers = message_list_tim_members_as_user_groups(message_list.get_tim_members())
        if message_sender:
            message_owners.append(message_sender.get_personal_group())
    elif message_list.archive_policy is ArchiveType.SECRET:
        # VIESTIM: There shouldn't be much to do with this archive policy? The list owners get ownership,
        #  and otherwise no one else sees it?
        pass

    # VIESTIM: If we don't provide at least one owner up front, then current user is set as owner. We don't want
    #  that, because in this context that is the anonymous user, and that raises an error.
    archive_doc = DocEntry.create(title=archive_title, path=archive_doc_path, owner_group=message_owners[0])

    # Add the rest of the message owners.
    if len(message_owners) > 1:
        archive_doc.block.add_rights(message_owners[1:], AccessType.owner)

    # Add view rights.
    archive_doc.block.add_rights(message_viewers, AccessType.view)

    return archive_doc


def archive_message(message_list: MessageListModel, message: MessageTIMversalis) -> None:
    """Archive a message for a message list.

    :param message_list: The message list where the archived message belongs.
    :param message: The message being archived.
    """
    # Archive policy of no archiving is a special case, where we abort immediately since these won't be archived at all.
    if message_list.archive_policy is ArchiveType.NONE:
        # VIESTIM: Do we need an exception here? Is it enough to just silently return?
        return

    archive_title = f"{message.title}-{get_current_time().strftime('%Y-%m-%d %H:%M:%S')}"
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

    # Set header information for archived message. The empty lines are needed to separate headers into their own lines.
    archive_doc.document.add_text(f"""Title: {message.title}
    
Sender: {message.sender}

Recipients: {message.recipients}

Message body:
""")

    # Set message body for archived message.
    # TODO: Check message list's only_text flag. If it is set, then wrap the message body in a code block?
    archive_doc.document.add_text(f"{message.message_body}")

    # If there is only one message, we don't need to add links to any other messages.
    if len(all_archived_messages):
        # TODO: Extract linkings to their own functions.
        sorted_messages = sorted(all_archived_messages, key=lambda document: document.block.created, reverse=True)
        previous_doc = sorted_messages[0]

        # Link the previous newest message and now archived message together.
        set_message_link(previous_doc.document, f"Next Message: {archive_doc.title}", archive_doc.url)
        set_message_link(archive_doc.document, f"Previous message: {previous_doc.title}", previous_doc.url)

    # TODO: Set proper rights to the document. The message sender owns the document. Owners of the list get at least a
    #  view right. Other rights depend on the message list's archive policy.
    db.session.commit()
    return


def set_message_link(link_to: Document, link_text: str, link_from_url: str) -> None:
    """Set link to a document from another document.

    :param link_to: The document where the link is appended.
    :param link_text: The text the link gets.
    :param link_from_url: The link to another document.
    """
    link = f"[{link_text}]({link_from_url})"
    link_to.add_text(link)
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
        title=original["subject"],  # TODO: shorten the subject, if it contains multiple Re: and Vs: prefixes?

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
    """Get the owners of a message list.

    :param mlist: The message list we want to know the owners.
    :return: A list of owners, as their personal user group.
    """
    manage_doc_block = Block.query.filter_by(id=mlist.manage_doc_id).one()
    return manage_doc_block.owners


def verify_list_owner(owner_candidate_ug: UserGroup, mlist: MessageListModel) -> bool:
    """Verify if a user is the owner of message list.

    :param owner_candidate_ug:
    :param mlist:
    :return: Return True if the owner candidate is a owner of a message list. Otherwise return false.
    """
    mlist_owners = get_message_list_owners(mlist)
    return owner_candidate_ug in mlist_owners


def create_management_doc(msg_list_model: MessageListModel, list_options: ListOptions) -> DocInfo:
    # TODO: Document should reside in owner's personal path.

    # VIESTIM: The management document is created on the message list creator's personal folder. This might be a good
    #  default, but if the owner is someone else than the creator then we have to handle that.

    # VIESTIM: We'll err on the side of caution and make sure the path is safe for the management doc.
    path_safe_list_name = remove_path_special_chars(list_options.name)
    path_to_doc = f'/{MESSAGE_LIST_DOC_PREFIX}/{path_safe_list_name}'

    doc = create_document(path_to_doc, list_options.name)

    # VIESTIM: We add the admin component to the document. This might have to be changed if the component is turned
    #  into a plugin.

    admin_component = """#- {allowangular="true"}
<tim-message-list-admin></tim-message-list-admin>
    """
    doc.document.add_text(admin_component)

    # Set the management doc for the message list.
    msg_list_model.manage_doc_id = doc.id

    return doc


def new_list(list_options: ListOptions) -> DocInfo:
    """Adds a new message list into the database and creates the list's management doc.

    :param list_options: The list information for creating a new message list.
    :return: The management document.
    """
    # VIESTIM: Check creation permission? Or should it be in the calling view function?
    msg_list = MessageListModel(name=list_options.name, archive=list_options.archive)
    if list_options.domain:
        msg_list.email_list_domain = list_options.domain
    db.session.add(msg_list)

    doc_info = create_management_doc(msg_list, list_options)

    db.session.commit()
    return doc_info


def set_message_list_notify_owner_on_change(message_list: MessageListModel,
                                            notify_owners_on_list_change_flag: Optional[bool]) -> None:
    """Set the notify list owner on list change flag for a list, and update necessary channels with this information.

    If the message list has an email list as a message channel, this will set the equilavent flag on the email list.

    :param message_list: The message list where the flag is being set.
    :param notify_owners_on_list_change_flag: A boolean flag. If True, then changes on the message list sends
    notifications to list owners. If False, notifications won't be sent.
    """
    if notify_owners_on_list_change_flag is None \
            or message_list.notify_owner_on_change == notify_owners_on_list_change_flag:
        return

    message_list.notify_owner_on_change = notify_owners_on_list_change_flag

    if message_list.email_list_domain:
        # Email lists have their own flag for notifying list owners for list changes.

        # VIESTIM: Until there is another type of notification system for message lists similiar to document changes,
        #  we rely on Mailman's notifications for list changes.
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_notify_owner_on_list_change(email_list, message_list.notify_owner_on_change)
    return


def set_message_list_member_can_unsubscribe(message_list: MessageListModel,
                                            can_unsubscribe_flag: Optional[bool]) -> None:
    """Set the list member's free unsubscription flag, and propagate that setting to channels that have own handling
    of unsubscription.

    If the message list has an email list as a message channel, this will set the equilavent flag on the email list.

    :param message_list: Message list where the flag is being set.
    :param can_unsubscribe_flag: A boolean value. For True, the member can unsubscribe on their own. For False, then
    the member can't unsubscribe from the list on their own.
    """
    if can_unsubscribe_flag is None or message_list.can_unsubscribe == can_unsubscribe_flag:
        return
    message_list.can_unsubscribe = can_unsubscribe_flag

    if message_list.email_list_domain:
        # Email list's have their own settings for unsubscription.
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_unsubscription_policy(email_list, can_unsubscribe_flag)
    return


def set_message_list_subject_prefix(message_list: MessageListModel, subject_prefix: Optional[str]) -> None:
    """Set the message list's subject prefix.

    If the message list has an email list as a message list, then set the subject prefix there also.

    :param message_list: The message list where the subject prefix is being set.
    :param subject_prefix: The prefix set for messages that go through the list.
    """
    if subject_prefix is None or message_list.subject_prefix == subject_prefix:
        return
    message_list.subject_prefix = subject_prefix

    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_subject_prefix(email_list, subject_prefix)
    return


def set_message_list_tim_users_can_join(message_list: MessageListModel, can_join_flag: Optional[bool]) -> None:
    """Set the flag controlling if TIM users can directly join this list.

    Because the behaviour that is controlled by the can_join_flag applies to TIM users, there is no message channel
    specific handling.

    :param message_list: Message list where the flag is being set.
    :param can_join_flag: If True, then TIM users can directly join this list, no moderation needed. If False, then TIM
    users can't direclty join this list and
    """
    if can_join_flag is None or message_list.tim_user_can_join == can_join_flag:
        return

    message_list.tim_user_can_join = can_join_flag
    return


def set_message_list_default_send_right(message_list: MessageListModel,
                                        default_send_right_flag: Optional[bool]) -> None:
    """Set the default message list new member send right flag.

    :param message_list: The message list where the flag is set.
    :param default_send_right_flag: For True, new members on the list get default send right. For False, new members
    don't get a send right.
    """
    if default_send_right_flag is None or message_list.default_send_right == default_send_right_flag:
        return
    message_list.default_send_right = default_send_right_flag
    return


def set_message_list_default_delivery_right(message_list: MessageListModel,
                                            default_delivery_right_flag: Optional[bool]) -> None:
    """Set the message list new member default delivery right.

    :param message_list: The message list where the flag is set.
    :param default_delivery_right_flag: For True, new members on the list get default delivery right. For False, new
    members don't get a delivery right.
    """
    if default_delivery_right_flag is None or message_list.default_delivery_right == default_delivery_right_flag:
        return
    message_list.default_delivery_right = default_delivery_right_flag
    return


def set_message_list_only_text(message_list: MessageListModel, only_text: Optional[bool]) -> None:
    """

    :param message_list:
    :param only_text:
    :return: None.
    """
    if only_text is None or message_list.only_text == only_text:
        return
    message_list.only_text = only_text

    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_only_text(email_list, only_text)
    return


def set_message_list_non_member_message_pass(message_list: MessageListModel,
                                             non_member_message_pass_flag: Optional[bool]) -> None:
    """Set message list's non member message pass flag.

    :param message_list: The message list where the flag is set.
    :param non_member_message_pass_flag: For True, sources outside the list can send messages to this list. If False,
     messages form sources outside the list will be hold for moderation.
    :return: None.
    """
    if non_member_message_pass_flag is None or message_list.non_member_message_pass == non_member_message_pass_flag:
        return
    message_list.non_member_message_pass = non_member_message_pass_flag
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_non_member_message_pass(email_list, non_member_message_pass_flag)
    return


def set_message_list_allow_attachments(message_list: MessageListModel, allow_attachments_flag: Optional[bool]) -> None:
    if allow_attachments_flag is None or message_list.allow_attachments == allow_attachments_flag:
        return

    message_list.allow_attachments = allow_attachments_flag
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_allow_attachments(email_list, allow_attachments_flag)
    return


def set_message_list_default_reply_type(message_list: MessageListModel,
                                        default_reply_type: Optional[ReplyToListChanges]) -> None:
    """

    :param message_list:
    :param default_reply_type:
    :return:
    """
    if default_reply_type is None or message_list.default_reply_type == default_reply_type:
        return

    message_list.default_reply_type = default_reply_type
    if message_list.email_list_domain:
        email_list = get_email_list_by_name(message_list.name, message_list.email_list_domain)
        set_email_list_default_reply_type(email_list, default_reply_type)
    return
