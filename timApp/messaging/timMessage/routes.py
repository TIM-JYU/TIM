import re
from dataclasses import dataclass
from datetime import datetime
from typing import Optional

from flask import Response
from isodate import datetime_isoformat
from sqlalchemy import tuple_

from timApp.auth.accesshelper import verify_edit_access, verify_manage_access
from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import create_document
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.documents import import_document_from_file
from timApp.document.translation.translation import Translation
from timApp.document.viewcontext import default_view_ctx
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.item.item import Item
from timApp.item.manage import TRASH_FOLDER_PATH
from timApp.messaging.timMessage.internalmessage_models import (
    InternalMessage,
    DisplayType,
    InternalMessageDisplay,
    InternalMessageReadReceipt,
)
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import (
    ok_response,
    json_response,
    text_response,
    csv_string,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import remove_path_special_chars, static_tim_doc

timMessage = TypedBlueprint("timMessage", __name__, url_prefix="/timMessage")


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
    recipients: list[str]


@dataclass
class TimMessageData:
    id: int
    sender: Optional[str]
    doc_path: str
    can_mark_as_read: bool
    can_reply: bool
    display_type: DisplayType
    message_body: str
    message_subject: str


@dataclass
class TimMessageReadReceipt:
    rcpt_id: int
    message_id: int
    user_id: int
    marked_as_read_on: datetime
    can_mark_as_read: bool


@timMessage.get("/get/<int:item_id>")
def get_tim_messages(item_id: int) -> Response:
    """
    Retrieve messages displayed for current based on item id and return them in json format.

    :param item_id: Identifier for document or folder where message is displayed
    :return: List of TIM messages to display
    """
    return json_response(get_tim_messages_as_list(item_id))


def get_tim_messages_as_list(item_id: int) -> list[TimMessageData]:
    """
    Retrieve messages displayed for current user based on item id and return them as a list.

    TODO: Once global messages are implemented, verify that user has view access
     to the document before displaying messages.

    :param item_id: Identifier for document or folder where message is displayed
    :return:
    """
    current_page_obj = DocEntry.find_by_id(item_id)
    if isinstance(current_page_obj, Translation):
        # Resolve to original file instead of translation file
        current_page_obj = current_page_obj.docentry
    if not current_page_obj:
        current_page_obj = Folder.get_by_id(item_id)
    if not current_page_obj:
        raise NotExist("No document or folder found")

    parent_paths = current_page_obj.parent_paths()  # parent folders

    # get message displays shown on current page or in parent folders
    displays = InternalMessageDisplay.query.outerjoin(
        Folder, Folder.id == InternalMessageDisplay.display_doc_id
    ).filter(
        (
            InternalMessageDisplay.usergroup
            == get_current_user_object().get_personal_group()
        )
        & (
            (InternalMessageDisplay.display_doc_id == current_page_obj.id)
            | (tuple_(Folder.location, Folder.name).in_(parent_paths))
        )
    )

    messages: list[InternalMessage] = []
    for display in displays:
        receipt = InternalMessageReadReceipt.query.filter_by(
            rcpt_id=display.usergroup_id, message_id=display.message_id
        ).first()
        expires = InternalMessage.query.filter_by(id=display.message_id).first()
        # message is shown if it has not been marked as read or replied to, and has not expired
        if receipt.marked_as_read_on is None and (
            expires.expires is None or expires.expires > datetime.now()
        ):
            messages.append(
                InternalMessage.query.filter_by(id=display.message_id).first()
            )

    full_messages = []
    for message in messages:
        document = DocEntry.query.filter_by(id=message.doc_id).first()
        if not document:
            raise NotExist(f"No document or folder found for the message {message.id}")

        sender = document.owners[0].name if document.owners else None
        if document.name.startswith(TRASH_FOLDER_PATH):
            continue

        body = document.document.get_paragraph(message.par_id).get_html(
            default_view_ctx
        )
        data = TimMessageData(
            id=message.id,
            sender=sender,
            doc_path=document.name,
            can_mark_as_read=message.can_mark_as_read,
            can_reply=message.reply,
            display_type=message.display_type,
            message_body=body,
            message_subject=document.title,
        )

        full_messages.append(data)

    return full_messages


@timMessage.get("/get_read_receipt/<int:doc_id>")
def get_read_receipt(doc_id: int) -> Response:
    """
    Retrieve read receipt object for the current user and message related to the given document id

    :param doc_id: Id of the message document
    :return:
    """
    message = InternalMessage.query.filter_by(doc_id=doc_id).first()
    receipt = InternalMessageReadReceipt.query.filter_by(
        rcpt_id=get_current_user_object().get_personal_group().id, message_id=message.id
    ).first()
    if not receipt:
        raise NotExist("Read receipt not found")

    receipt_data = TimMessageReadReceipt(
        rcpt_id=receipt.rcpt_id,
        message_id=message.id,
        user_id=receipt.user_id,
        marked_as_read_on=receipt.marked_as_read_on,
        can_mark_as_read=message.can_mark_as_read,
    )

    return json_response(receipt_data)


# Regex pattern for url verification.
URL_PATTERN = re.compile(
    r"https?://[a-z0-9.-]*/(show_slide|view|teacher|velp|answers|lecture|review|slide)/"
)


@timMessage.post("/url_check")
def check_urls(urls: str) -> Response:
    """
    Checks if given URLS's exist in TIM and that user has right to post TIM message to them

    :param urls: Urls where user wishes to post TIM message
    :return: Shortened urls to show the user in the UI, or an error message
    """
    url_list = list(
        filter(None, urls.splitlines())
    )  # turn URL string into a list with empty values (new lines) removed
    valid_urls: list[str] = []
    error_message: str = ""
    status_code: int

    for url in url_list:
        url = url.strip()  # remove leading and trailing whitespaces
        if url.endswith("/"):
            url = url[:-1]
        hashtag_index = url.find("#")  # remove anchors
        if hashtag_index != -1:
            url = url[:hashtag_index]
        if URL_PATTERN.search(url):  # check if url matches the TIM urls' pattern
            shortened_url = URL_PATTERN.sub("", url)
        else:
            shortened_url = url
        document = DocEntry.find_by_path(shortened_url)  # check if url exists in TIM
        if document is None:
            document = Folder.find_by_path(shortened_url)
        if document is None:
            error_message = url + " was not found in TIM"
            status_code = 404
            break
        try:  # check if user has permission to edit the url
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


@timMessage.post("/send")
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

    tim_message = InternalMessage(
        can_mark_as_read=options.readReceipt,
        reply=options.reply,
        expires=options.expires,
        replies_to=options.repliesTo,
    )
    message_doc = create_tim_message(tim_message, options, message)
    db.session.add(tim_message)

    pages = get_display_pages(options.pageList.splitlines())
    recipients = get_recipient_users(message.recipients)
    create_message_displays(tim_message, pages, recipients)
    if recipients:
        create_read_receipts(tim_message, recipients)

    db.session.commit()

    return json_response({"docPath": message_doc.path})


def create_tim_message(
    tim_message: InternalMessage, options: MessageOptions, message_body: MessageBody
) -> DocInfo:
    """
    Creates a TIM document for the message to the TIM messages folder at TIM's root.

    :param tim_message: InternalMessage object
    :param options: Options related to the message
    :param message_body: Message subject, contents and list of recipients
    :return: The created Document object
    """
    sender = get_current_user_object()
    recipient_users = get_recipient_users(message_body.recipients)
    message_folder_path = "messages/tim-messages"

    message_subject = message_body.messageSubject
    timestamp = datetime.now()  # add timestamp to document path to make it unique
    message_path = remove_path_special_chars(f"{timestamp}-{message_subject}")

    check_messages_folder_path("messages", message_folder_path)
    message_doc = create_document(
        f"{message_folder_path}/{message_path}", message_subject
    )
    message_doc.block.add_rights([sender.get_personal_group()], AccessType.owner)
    message_doc.block.add_rights(recipient_users, AccessType.view)

    update_tim_msg_doc_settings(message_doc, sender, message_body)
    message_par = message_doc.document.add_paragraph(message_body.messageBody)
    message_doc.document.add_paragraph(
        "<manage-read-receipt></manage-read-receipt>", attrs={"allowangular": "true"}
    )
    tim_message.block = message_doc.block
    tim_message.par_id = message_par.get_id()
    tim_message.display_type = (
        DisplayType.STICKY if options.important else DisplayType.TOP_OF_PAGE
    )

    return message_doc


@timMessage.post("/reply")
def reply_to_tim_message(options: ReplyOptions, messageBody: MessageBody) -> Response:
    messageOptions = MessageOptions(
        options.messageChannel,
        False,
        True,
        options.archive,
        options.pageList,
        options.readReceipt,
        False,
        get_current_user_object().name,
        get_current_user_object().email,
        options.repliesTo,
    )
    recipient = User.get_by_name(messageBody.recipients.pop())
    if recipient:
        recipient_email = recipient.email
    else:
        raise NotExist("Recipient not found")

    message = MessageBody(
        messageBody.messageBody, messageBody.messageSubject, [recipient_email]
    )

    return send_message_or_reply(messageOptions, message)


@timMessage.post("/mark_as_read")
def mark_as_read(message_id: int) -> Response:
    """
    Marks given message as read in database.
    Expects that message receiver and marker are the same person.

    :param message_id: Id of given message
    :return:
    """
    verify_logged_in()

    marker = get_current_user_object().get_personal_group().id

    read_receipt = InternalMessageReadReceipt.query.filter_by(
        rcpt_id=marker, message_id=message_id
    ).first()
    if read_receipt is None:
        raise RouteException
    read_receipt.user_id = get_current_user_object().id
    read_receipt.marked_as_read_on = datetime.now()
    db.session.add(read_receipt)
    db.session.commit()

    return ok_response()


@timMessage.post("/cancel_read_receipt")
def cancel_read_receipt(message_id: int) -> Response:
    """
    Removes read receipt date and the user who marked it from the database entry.

    :param message_id: Message identifier
    :return:
    """
    verify_logged_in()

    receipt = InternalMessageReadReceipt.query.filter_by(
        rcpt_id=get_current_user_object().get_personal_group().id, message_id=message_id
    ).one()
    receipt.user_id = None
    receipt.marked_as_read_on = None
    db.session.commit()

    return ok_response()


@timMessage.get("/readReceipts")
def get_read_receipts(
    message_doc: int,
    include_read: bool = False,
    include_unread: bool = False,
    separator: str = ";",
) -> Response:
    verify_logged_in()
    doc = DocEntry.find_by_id(message_doc)
    if not doc:
        raise NotExist("No document found")
    verify_manage_access(doc)

    read_users = (
        db.session.query(
            InternalMessageReadReceipt.user_id,
            InternalMessageReadReceipt.marked_as_read_on,
        )
        .join(InternalMessage)
        .filter(InternalMessage.doc_id == doc.id)
    )

    read_user_map: dict[int, datetime] = {
        user_id: read_time for user_id, read_time in read_users
    }

    all_recipients = (
        User.query.join(UserGroupMember, User.active_memberships)
        .join(
            InternalMessageDisplay,
            InternalMessageDisplay.usergroup_id == UserGroupMember.usergroup_id,
        )
        .join(InternalMessage)
        .filter(InternalMessage.doc_id == doc.id)
    )

    data = [["id", "email", "name", "read_on"]]

    for u in all_recipients:
        read_time = ""
        if u.id in read_user_map:
            if not include_read:
                continue
            read_time = datetime_isoformat(read_user_map[u.id])
        elif not include_unread:
            continue
        data.append([u.id, u.email, u.real_name, read_time])

    return text_response(csv_string(data, "excel", separator))


def get_recipient_users(recipients: list[str]) -> list[UserGroup]:
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


def get_display_pages(pagelist: list[str]) -> list[Item]:
    """
    Finds folders and documents based on their paths.

    :param pagelist: list of paths
    :return: list of folders and documents
    """
    pages: list[Item] = []
    for page in pagelist:
        folder = Folder.find_by_path(page)
        if folder:
            pages.append(folder)
            continue

        doc = DocEntry.find_by_path(page)
        if doc:
            pages.append(doc)

    return pages


def check_messages_folder_path(
    msg_folder_path: str, tim_msg_folder_path: str
) -> Folder:
    """
    Checks if the /messages/tim-messages folder exists and if not, creates it. All users
     get view access to /messages folder and edit access to /messages/tim-messages folder
     so that documents for sent messages can be created. Also creates the preamble for
     message documents.

    :param msg_folder_path: path for /messages
    :param tim_msg_folder_path: path for /messages/tim-messages
    :return: /messages/tim-messages folder
    """
    msg_folder = Folder.find_by_location(msg_folder_path, "messages")
    admin_group = UserGroup.get_admin_group()

    if not msg_folder:
        msg_folder = Folder.create(
            msg_folder_path,
            admin_group,
            title="Messages",
            creation_opts=FolderCreationOptions(apply_default_rights=True),
        )
        msg_block = msg_folder.block
        if msg_block:
            msg_block.add_rights([UserGroup.get_logged_in_group()], AccessType.view)

    tim_msg_folder = Folder.find_by_location(tim_msg_folder_path, "tim-messages")

    if not tim_msg_folder:
        tim_msg_folder = Folder.create(
            tim_msg_folder_path,
            admin_group,
            title="TIM messages",
            creation_opts=FolderCreationOptions(apply_default_rights=True),
        )
        tim_msg_block = tim_msg_folder.block
        if tim_msg_block:
            tim_msg_block.add_rights([UserGroup.get_logged_in_group()], AccessType.edit)

    tim_msg_preambles = Folder.find_by_location(
        f"{tim_msg_folder_path}/templates/preambles", "preambles"
    )

    if not tim_msg_preambles:
        tim_msg_templates = Folder.create(
            f"{tim_msg_folder_path}/templates",
            admin_group,
            title="templates",
            creation_opts=FolderCreationOptions(apply_default_rights=True),
        )
        tim_msg_preambles = Folder.create(
            f"{tim_msg_folder_path}/templates/preambles",
            admin_group,
            title="preambles",
            creation_opts=FolderCreationOptions(apply_default_rights=True),
        )

        tim_msg_templates_block = tim_msg_templates.block
        if tim_msg_templates_block:
            tim_msg_templates_block.add_rights(
                [UserGroup.get_logged_in_group()], AccessType.view
            )

        tim_msg_preambles_block = tim_msg_preambles.block
        if tim_msg_preambles_block:
            tim_msg_preambles_block.add_rights(
                [UserGroup.get_logged_in_group()], AccessType.view
            )

    preamble_path = f"{tim_msg_folder_path}/templates/preambles/preamble"
    tim_msg_preamble = DocEntry.find_by_path(preamble_path)

    if not tim_msg_preamble:
        tim_msg_preamble = import_document_from_file(
            static_tim_doc("initial/tim_msg_preamble.md"),
            preamble_path,
            admin_group,
            title="preamble",
        )

        tim_msg_preamble.block.add_rights(
            [UserGroup.get_logged_in_group()], AccessType.view
        )

    return tim_msg_folder


def update_tim_msg_doc_settings(
    message_doc: DocInfo, sender: User, message_body: MessageBody
) -> None:
    """
    Sets the message information into the preamble macros.

    :param message_doc: TIM message document
    :param sender: Sender user
    :param message_body: Message body
    :return:
    """
    s = message_doc.document.get_settings().get_dict().get("macros", {})
    s["subject"] = message_body.messageSubject
    s["sendername"] = sender.name
    s["senderemail"] = sender.email
    s["recipients"] = message_body.recipients

    message_doc.document.add_setting("macros", s)


def create_message_displays(
    msg: InternalMessage, pages: list[Item], recipients: list[UserGroup]
) -> None:
    """
    Creates InternalMessageDisplay entries for all recipients and display pages.

    :param msg: Message
    :param pages: List of pages where message is displayed
    :param recipients: List of message recipients
    :return:
    """
    if pages and recipients:
        for page in pages:
            for rcpt in recipients:
                display = InternalMessageDisplay(
                    message=msg, usergroup=rcpt, display_block=page.block
                )
                db.session.add(display)

    if pages and not recipients:
        for page in pages:
            display = InternalMessageDisplay(message=msg, display_block=page.block)
            db.session.add(display)

    if not pages and recipients:
        for rcpt in recipients:
            display = InternalMessageDisplay(message=msg, usergroup=rcpt)
            db.session.add(display)

    if not pages and not recipients:
        display = InternalMessageDisplay(message=msg)
        db.session.add(display)


def create_read_receipts(msg: InternalMessage, recipients: list[UserGroup]) -> None:
    """
    Create InternalMessageReadReceipt entries for all recipients.

    :param msg: Message
    :param recipients: Message recipients
    :return:
    """
    for recipient in recipients:
        readreceipt = InternalMessageReadReceipt(recipient=recipient, message=msg)
        db.session.add(readreceipt)
