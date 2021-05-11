import re
from dataclasses import dataclass
from typing import Optional, List
from datetime import datetime

from flask import Response

from timApp.util.flask.requesthelper import RouteException
from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.auth.accesshelper import verify_edit_access
from timApp.document.create_item import create_document
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.document import Document
from timApp.document.viewcontext import default_view_ctx
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.item.item import Item
from timApp.messaging.timMessage.internalmessage_models import InternalMessage, DisplayType, InternalMessageDisplay, \
    InternalMessageReadReceipt
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import ok_response, json_response, error_generic
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
    repliesTo: Optional[int] = None
    expires: Optional[datetime] = None


@dataclass
class ReplyOptions:
    archive: bool
    messageChannel: bool
    pageList: str
    recipient: str
    readReceipt: bool = True
    repliesTo: Optional[int] = None


@dataclass
class MessageBody:
    messageBody: str
    messageSubject: str
    recipients: List[str]  # VIESTIM: find recipient by email or some other identifier?


@dataclass
class TimMessageData:
    # Information about the message sent to browser
    id: int
    sender: str
    doc_id: int
    par_id: str
    can_mark_as_read: bool
    can_reply: bool
    display_type: int
    message_body: str
    message_subject: str
    recipients: List[str]


@timMessage.route("/get/<int:item_id>", methods=['GET'])
def get_tim_messages(item_id: int) -> Response:
    """
    Retrieve messages displayed for current based on item id and return them in json format.

    :param item_id: Identifier for document or folder where message is displayed
    :return:
    """
    fullmessages = get_tim_messages_as_list(item_id)

    return json_response(fullmessages)


def get_tim_messages_as_list(item_id: int) -> List[TimMessageData]:
    """
    Retrieve messages displayed for current user based on item id and return them as a list.

    :param item_id: Identifier for document or folder where message is displayed
    :return:
    """
    displays = InternalMessageDisplay.query.filter_by(usergroup_id=get_current_user_object().get_personal_group().id,
                                                      display_doc_id=item_id).all()

    replies = InternalMessage.query.filter(InternalMessage.replies_to.isnot(None)).with_entities(InternalMessage.replies_to).all()
    replies_to_ids =  [a for a, in replies] # list of messages that have been replied to

    messages = []
    recipients = []
    for display in displays:
        receipt = InternalMessageReadReceipt.query.filter_by(rcpt_id=display.usergroup_id, message_id=display.message_id).first()
        expires = InternalMessage.query.filter_by(id=display.message_id).first()
        # message is shown if it has not been marked as read or replied to, and has not expired
        if receipt.marked_as_read_on is None and display.message_id not in replies_to_ids and (expires.expires is None or expires.expires > datetime.now()):
            messages.append(InternalMessage.query.filter_by(id=display.message_id).first())
            recipients.append(UserGroup.query.filter_by(id=display.usergroup_id).first())

    fullmessages = []
    for message in messages:
        document = DocEntry.find_by_id(message.doc_id)
        if not document:
            return error_generic('Message document not found', 404)
        fullmessages.append(TimMessageData(id=message.id, sender=document.owners.pop().name, doc_id=message.doc_id,
                                           par_id=message.par_id, can_mark_as_read=message.can_mark_as_read,
                                           can_reply=message.reply, display_type=message.display_type,
                                           message_body=Document.get_paragraph(document.document,
                                                                               message.par_id).get_html(
                                               default_view_ctx),
                                           message_subject=document.title, recipients=recipients))

    return fullmessages


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
        if url.endswith("/"):
            url = url[:-1]
        hashtag_index = url.find("#")  #remove anchors
        if hashtag_index != -1:
            url = url[:hashtag_index]
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
            error_message = "You don't have permission to post TIM message to " + url
            status_code = 401

    if error_message:
        return json_response({"error": error_message}, status_code)
    else:
        valid_urls_string = "\n".join(valid_urls)  # turn URL list into a string again
        return json_response({"shortened_urls": valid_urls_string}, 200)


@timMessage.route("/send", methods=['POST'])
def send_tim_message(options: MessageOptions, message: MessageBody) -> Response:
    return send_message_or_reply(options, message)


def send_message_or_reply(options: MessageOptions, message: MessageBody) -> Response:
    """
        Creates a new TIM message and saves it to database.

        :param options: Options related to the message
        :param message: Message subject, contents and sender
        :return:
        """
    verify_logged_in()

    tim_message = InternalMessage(can_mark_as_read=options.readReceipt, reply=options.reply, expires=options.expires, replies_to=options.repliesTo)
    create_tim_message(tim_message, options, message)
    db.session.add(tim_message)
    db.session.flush()

    pages = get_display_pages(options.pageList.splitlines())
    recipients = get_recipient_users(message.recipients)
    create_message_displays(tim_message.id, pages, recipients)
    if recipients:
        create_read_receipts(tim_message.id, recipients)

    db.session.commit()

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

    tim_message.block = message_doc.block
    tim_message.par_id = message_par.get_id()

    if options.important:
        # Important messages are interpreted as 'sticky' display type
        tim_message.display_type = DisplayType.STICKY  # TODO actual functionality
    else:
        tim_message.display_type = DisplayType.TOP_OF_PAGE  # default display type

    return message_doc


@timMessage.route("/reply", methods=['POST'])
def reply_to_tim_message(options: ReplyOptions, messageBody: MessageBody) -> Response:

    # VIESTIM: add option replies_to to MessageOptions (and column to internalmessage table in db, save original message's id here)

    messageOptions = MessageOptions(options.messageChannel, False, True, options.archive, options.pageList, options.readReceipt, False, get_current_user_object().name, get_current_user_object().email, options.repliesTo)
    message = messageBody

    return send_message_or_reply(messageOptions, message)


@timMessage.route("/mark_as_read", methods=['POST'])
def mark_as_read(message_id: int) -> Response:
    """
        Marks given message as read in database.
        Expects that message receiver and marker are the same person.

        :param message_id: Id of given message
        :return:
        """
    verify_logged_in()

    marker = get_current_user_object().get_personal_group().id

    read_receipt = InternalMessageReadReceipt.query.filter_by(rcpt_id=marker, message_id=message_id).first()
    if read_receipt is None:
        raise RouteException
    read_receipt.user_id = get_current_user_object().id
    read_receipt.marked_as_read_on = datetime.now()
    db.session.add(read_receipt)
    db.session.commit()

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
            tim_msg_block.add_rights([UserGroup.get_logged_in_group()], AccessType.edit)

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
