import re
from dataclasses import dataclass
from typing import Optional, List
from datetime import datetime

from flask import Response

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.auth.accesshelper import verify_edit_access
from timApp.document.create_item import create_document
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.item import Item
from timApp.messaging.timMessage.internalmessage_models import InternalMessage, DisplayType, InternalMessageDisplay, \
    InternalMessageReadReceipt
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import ok_response, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import remove_path_special_chars


timMessage = TypedBlueprint('timMessage', __name__, url_prefix='/timMessage')


@dataclass
class MessageOptions:
    # Options regarding TIM messages
    messageChannel: bool
    important: bool
    isPrivate: bool
    archive: bool
    pageList: str
    readReceipt: bool
    reply: bool
    sender: str
    senderEmail: str
    expires: Optional[datetime] = None

@dataclass
class MessageBody:
    messageBody: str
    messageSubject: str
    recipients: List[str]  # VIESTIM: find recipient by email or some other identifier?


@timMessage.route("/url_check", methods=['POST'])
def check_urls(urls: str) -> Response:
    """
    Checks if given URLS's exist in TIM and that user has right to post TIM message to them

    :param urls: Urls where user wishes to post TIM message
    :return: Shortened urls to show the user in the UI, or an error message
    """
    url_list = list(filter(None, urls.splitlines()))  # turn URL string into a list with empty values (new lines) removed
    valid_urls: List[str] = []
    error_message: str = ""
    status_code: int

    for url in url_list:
        url = url.strip()  #remove leading and trailing whitespaces
        regex = "https?://[a-z0-9.-]*/(show_slide|view|teacher|velp|answers|lecture|review|slide)/"
        if re.search(regex, url):  # check if url matches the TIM urls' pattern
            shortened_url = re.sub(regex, "", url)
        else:
            shortened_url = url
        document = DocEntry.find_by_path(shortened_url) # check if url exists in TIM
        if document is None:
            document = Folder.find_by_path(shortened_url)
        if document is None:
            error_message = url + " was not found in TIM"
            status_code = 404
            break
        try: # check if user has permission to edit the url
            verify_edit_access(document)
            valid_urls.append(shortened_url)
        except Exception:
            error_message = "You don't have permission to post TIM message to this page"
            status_code = 401

    if error_message:
        return json_response({"error": error_message}, status_code)
    else:
        valid_urls_string = "\n".join(valid_urls)  # turn URL list into a string again
        return json_response({"shortened_urls": valid_urls_string}, 200)


@timMessage.route("/send", methods=['POST'])
def send_tim_message(options: MessageOptions, message: MessageBody) -> Response:
    """
    Creates a new TIM message and saves it to database.

    :param options: Options related to the message
    :param message: Message subject, contents and sender
    :return:
    """
    verify_logged_in()

    tim_message = InternalMessage(can_mark_as_read=options.readReceipt, reply=options.reply)
    create_tim_message(tim_message, options, message)
    db.session.add(tim_message)
    db.session.commit()

    pages = get_display_pages(options.pageList.splitlines())
    recipients = get_recipient_users(message.recipients)
    create_message_displays(tim_message.id, pages, recipients)
    if recipients:
        create_read_receipts(tim_message.id, recipients)

    return ok_response()


def create_tim_message(tim_message: InternalMessage, options: MessageOptions, message_body: MessageBody) -> DocInfo:
    """
    Creates a TIM document for the message to the TIM messages folder at TIM's root.

    :param tim_message: InternalMessage object
    :param options: Options related to the message
    :param message_body: Message subject, contents and list of recipients
    :return: The created Document object
    """
    sender = get_current_user_object()
    recipient_users = get_recipient_users(message_body.recipients)
    message_folder_path = '/messages/tim-messages'

    message_subject = message_body.messageSubject
    timestamp = datetime.now()  # add timestamp to document path to make it unique
    message_path = remove_path_special_chars(f'{timestamp}-{message_subject}')

    check_messages_folder_path('/messages', message_folder_path)

    message_doc = create_document(f'{message_folder_path}/{message_path}',
                                  message_subject)

    message_doc.block.add_rights([sender.get_personal_group()], AccessType.owner)
    message_doc.block.add_rights(recipient_users, AccessType.view)

    message_doc.document.add_paragraph(f'# {message_subject}')
    message_doc.document.add_paragraph(f'**From:** {sender.name}, {sender.email}')
    message_doc.document.add_paragraph(f'**To:** {message_body.recipients}')
    message_par = message_doc.document.add_paragraph(message_body.messageBody)

    tim_message.doc_id = message_doc.id
    tim_message.par_id = message_par.get_id()
    if options.important:
        # Important messages are interpreted as 'sticky' display type
        tim_message.display_type = DisplayType.STICKY  # TODO actual functionality
    else:
        tim_message.display_type = DisplayType.TOP_OF_PAGE  # default display type

    tim_message.block = Block(type_id=0, created=message_doc.block.created)

    return message_doc


@timMessage.route("/reply", methods=['POST'])
def reply_to_tim_message(options: MessageOptions, message: MessageBody) -> Response:
    # TODO handle replying to message
    return ok_response()


def get_recipient_users(recipients: List[str]) -> List[UserGroup]:
    """
    Finds UserGroup objects of recipients based on their email

    :param recipients: list of recipients' emails
    :return: list of recipient UserGroups
    """
    users = []
    for rcpt in recipients:
        user = User.get_by_email(rcpt)
        if user:
            users.append(UserGroup.get_by_name(user.name))

    return users


def get_display_pages(pagelist: List[str]) -> List[Item]:
    """
    Finds folders and documents based on their paths.

    :param pagelist: list of paths
    :return: list of folders and documents
    """
    pages: List[Item] = []
    for page in pagelist:
        folder = Folder.find_by_path(page)
        if folder:
            pages.append(folder)
            continue

        doc = DocEntry.find_by_path(page)
        if doc:
            pages.append(doc)

    return pages


def check_messages_folder_path(msg_folder_path: str, tim_msg_folder_path: str) -> Folder:
    """
    Checks if the /messages/tim-messages folder exists and if not, creates it and
    adds view rights to logged in users.

    :param msg_folder_path: path for /messages
    :param tim_msg_folder_path: path for /messages/tim-messages
    :return: /messages/tim-messages folder
    """
    msg_folder = Folder.find_by_location(msg_folder_path, 'messages')

    if not msg_folder:
        msg_folder = Folder.create(msg_folder_path, UserGroup.get_admin_group(), title='Messages',
                                   creation_opts=FolderCreationOptions(apply_default_rights=True))
        msg_block = msg_folder.block
        if msg_block:
            msg_block.add_rights([UserGroup.get_logged_in_group()], AccessType.view)

    tim_msg_folder = Folder.find_by_location(tim_msg_folder_path, 'tim-messages')

    if not tim_msg_folder:
        tim_msg_folder = Folder.create(tim_msg_folder_path, UserGroup.get_admin_group(), title='TIM messages',
                                       creation_opts=FolderCreationOptions(apply_default_rights=True))
        tim_msg_block = tim_msg_folder.block
        if tim_msg_block:
            tim_msg_block.add_rights([UserGroup.get_logged_in_group()], AccessType.view)

    return tim_msg_folder


def create_message_displays(msg_id: int, pages: List[Item], recipients: List[UserGroup]) -> None:
    """
    Creates InternalMessageDisplay entries for all recipients and display pages.

    :param msg_id: Message identifier
    :param pages: List of pages where message is displayed
    :param recipients: List of message recipients
    :return:
    """
    if pages and recipients:
        for page in pages:
            for rcpt in recipients:
                display = InternalMessageDisplay()
                display.message_id = msg_id
                display.usergroup_id = rcpt.id
                display.display_doc_id = page.id
                db.session.add(display)

    if pages and not recipients:
        for page in pages:
            display = InternalMessageDisplay()
            display.message_id = msg_id
            display.display_doc_id = page.id
            db.session.add(display)

    if not pages and recipients:
        for rcpt in recipients:
            display = InternalMessageDisplay()
            display.message_id = msg_id
            display.usergroup_id = rcpt.id
            db.session.add(display)

    if not pages and not recipients:
        display = InternalMessageDisplay()
        display.message_id = msg_id
        db.session.add(display)

    db.session.commit()


def create_read_receipts(msg_id: int, recipients: List[UserGroup]) -> None:
    """
    Create InternalMessageReadReceipt entries for all recipients.

    :param msg_id: Message identifier
    :param recipients: Message recipients
    :return:
    """
    for recipient in recipients:
        readreceipt = InternalMessageReadReceipt(rcpt_id=recipient.id, message_id=msg_id)
        db.session.add(readreceipt)

    db.session.commit()
