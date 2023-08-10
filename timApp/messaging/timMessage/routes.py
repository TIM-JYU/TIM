import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Sequence

from flask import Response
from isodate import datetime_isoformat
from sqlalchemy import tuple_, select, false
from sqlalchemy.orm import contains_eager

from timApp.auth.accesshelper import (
    verify_edit_access,
    verify_manage_access,
    verify_admin,
)
from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import (
    get_current_user_object,
    logged_in,
)
from timApp.document.create_item import create_document
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.documents import import_document_from_file
from timApp.document.hide_names import is_hide_names
from timApp.document.translation.translation import Translation
from timApp.document.viewcontext import default_view_ctx
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.item.deleting import TRASH_FOLDER_PATH
from timApp.item.item import Item
from timApp.messaging.messagelist.messagelist_models import (
    MessageListModel,
    MessageListTimMember,
)
from timApp.messaging.timMessage.internalmessage_models import (
    InternalMessage,
    DisplayType,
    InternalMessageDisplay,
    InternalMessageReadReceipt,
)
from timApp.timdb.sqa import db, run_sql
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.flask.requesthelper import NotExist, RouteException
from timApp.util.flask.responsehelper import (
    ok_response,
    json_response,
    text_response,
    csv_string,
)
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import (
    remove_path_special_chars,
    static_tim_doc,
    get_current_time,
)

tim_message = TypedBlueprint("timMessage", __name__, url_prefix="/timMessage")


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
    repliesTo: int | None = None
    expires: datetime | None = None


@dataclass
class ReplyOptions:
    archive: bool
    messageChannel: bool
    pageList: str
    recipient: str
    readReceipt: bool = True
    repliesTo: int | None = None


@dataclass
class MessageBody:
    messageBody: str
    messageSubject: str
    recipients: list[str] | None = None


@dataclass
class TimMessageData:
    id: int
    created: datetime
    sender: str | None
    doc_path: str
    can_mark_as_read: bool
    can_reply: bool
    display_type: DisplayType
    message_body: str
    message_subject: str


@dataclass
class TimMessageReadReceipt:
    message_id: int
    user_id: int
    marked_as_read_on: datetime | None
    can_mark_as_read: bool


@tim_message.get("/get")
def get_global_messages() -> Response:
    """
    Retrieve global messages return them in json format.

    :return: List of TIM messages to display
    """
    return json_response(get_tim_messages_as_list())


@tim_message.get("/get/<int:item_id>")
def get_tim_messages(item_id: int) -> Response:
    """
    Retrieve messages displayed for current based on item id and return them in json format.

    :param item_id: Identifier for document or folder where message is displayed
    :return: List of TIM messages to display
    """
    return json_response(get_tim_messages_as_list(item_id))


@tim_message.post("/expire/<int:message_doc_id>")
def expire_tim_message(message_doc_id: int) -> Response:
    """
    Expire a TIM message.

    :param message_doc_id: Document ID of the message to expire.
    :return: OK response if message was successfully expired.
    """

    internal_message: InternalMessage | None = (
        run_sql(select(InternalMessage).filter_by(doc_id=message_doc_id))
        .scalars()
        .first()
    )
    if not internal_message:
        raise NotExist("Message not found")
    verify_manage_access(internal_message.block)
    internal_message.expires = get_current_time()
    db.session.commit()
    return ok_response()


def get_tim_messages_as_list(item_id: int | None = None) -> list[TimMessageData]:
    """
    Retrieve messages displayed for current user based on item id and return them as a list.

    :param item_id: Identifier for document or folder where message is displayed. If None, global messages are returned.
    :return: List of TIM messages to display
    """

    # TODO: Add logic for anon users to see and hide global messages
    if not logged_in():
        return []

    now = get_current_time()
    is_global = (InternalMessageDisplay.usergroup_id == None) & (
        InternalMessageDisplay.display_doc_id == None
    )
    is_user_specific: Any = false()
    can_see = (InternalMessageReadReceipt.marked_as_read_on == None) & (
        (InternalMessage.expires == None) | (InternalMessage.expires > now)
    )

    if item_id is not None:
        current_page_obj = DocEntry.find_by_id(item_id)
        if isinstance(current_page_obj, Translation):
            # Resolve to original file instead of translation file
            current_page_obj = current_page_obj.docentry
        if not current_page_obj:
            current_page_obj = Folder.get_by_id(item_id)
        if not current_page_obj:
            raise NotExist("No document or folder found")

        parent_paths = current_page_obj.parent_paths()  # parent folders
        current_group_ids = get_current_user_object().effective_group_ids

        is_user_specific = (
            InternalMessageDisplay.usergroup_id.in_(current_group_ids)
        ) & (
            (InternalMessageDisplay.display_doc_id == current_page_obj.id)
            | (tuple_(Folder.location, Folder.name).in_(parent_paths))
        )

    cur_user = get_current_user_object()
    stmt = (
        select(InternalMessage)
        .join(InternalMessageDisplay)
        .outerjoin(Folder, Folder.id == InternalMessageDisplay.display_doc_id)
        .outerjoin(
            InternalMessageReadReceipt,
            (InternalMessageReadReceipt.message_id == InternalMessage.id)
            # Do this in outer join because message can be seen if no receipt is found or receipt is not marked as read
            # With outer join, both cases are covered (marked_as_read_on becomes NULL)
            & (InternalMessageReadReceipt.user_id == cur_user.id),
        )
        .options(contains_eager(InternalMessage.readreceipts))
        .filter((is_global | is_user_specific) & can_see)
    )

    # Make unique because each message contains multiple readreceipts
    messages: Sequence[InternalMessage] = run_sql(stmt).unique().scalars().all()

    full_messages = []
    for message in messages:
        document = DocEntry.find_by_id(message.doc_id)
        if not document:
            raise NotExist(f"No document or folder found for the message {message.id}")

        sender = document.owners[0].name if document.owners else None
        if document.path.startswith(TRASH_FOLDER_PATH):
            continue

        body = document.document.get_paragraph(message.par_id).get_html(
            default_view_ctx
        )
        data = TimMessageData(
            id=message.id,
            created=message.created,
            sender=sender,
            doc_path=document.path,
            can_mark_as_read=message.can_mark_as_read,
            can_reply=message.reply,
            display_type=message.display_type,
            message_body=body,
            message_subject=document.title,
        )

        full_messages.append(data)

        if message.readreceipts:
            # Note: previous contains_eager will force message.readreceipts to contain only receipt of current user
            for read_receipt in message.readreceipts:
                read_receipt.last_seen = now
        else:
            receipt = InternalMessageReadReceipt(
                message=message, user=cur_user, last_seen=now
            )
            db.session.add(receipt)

    db.session.commit()

    return full_messages


@tim_message.get("/get_read_receipt/<int:doc_id>")
def get_read_receipt(doc_id: int) -> Response:
    """
    Retrieve read receipt object for the current user and message related to the given document id

    :param doc_id: Id of the message document
    :return:
    """
    message = (
        run_sql(select(InternalMessage).filter_by(doc_id=doc_id)).scalars().first()
    )
    if not message:
        raise NotExist("No active messages for the document found")
    receipt = InternalMessageReadReceipt.get_for_user(
        get_current_user_object(), message
    )
    if not receipt:
        return json_response({"receipt": None, "expires": message.expires})

    receipt_data = TimMessageReadReceipt(
        message_id=message.id,
        user_id=receipt.user_id,
        marked_as_read_on=receipt.marked_as_read_on,
        can_mark_as_read=message.can_mark_as_read,
    )

    return json_response({"receipt": receipt_data, "expires": message.expires})


# Regex pattern for url verification.
URL_PATTERN = re.compile(
    r"https?://[a-z0-9.-]*/(show_slide|view|teacher|velp|answers|lecture|review|slide)/"
)


@tim_message.post("/url_check")
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


@tim_message.post("/send")
def send_tim_message(message: MessageBody, options: MessageOptions) -> Response:
    is_global = message.recipients is None
    if is_global:
        verify_admin()
        options.messageChannel = False
        options.isPrivate = True
        options.archive = False
        options.pageList = ""
        options.readReceipt = True
        options.reply = False
        options.sender = ""
        options.senderEmail = ""

    return send_message_or_reply(message, options)


def send_message_or_reply(message: MessageBody, options: MessageOptions) -> Response:
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
    recipients = get_recipient_users(message.recipients)
    message_doc = create_tim_message(tim_message, options, message, recipients)

    pages = get_display_pages(options.pageList.splitlines())
    create_message_displays(tim_message, pages, recipients)

    db.session.add(tim_message)
    db.session.commit()

    return json_response({"docPath": message_doc.path})


def create_tim_message(
    tim_message: InternalMessage,
    options: MessageOptions,
    message_body: MessageBody,
    message_viewers: list[UserGroup] | None = None,
) -> DocInfo:
    """
    Creates a TIM document for the message to the TIM messages folder at TIM's root.

    :param tim_message: InternalMessage object
    :param options: Options related to the message
    :param message_body: Message subject, contents and list of recipients
    :param message_viewers: Groups that are allowed to view the message. If None, all recipients can.
    :return: The created Document object
    """
    recipients = (
        message_viewers
        if message_viewers is not None
        else get_recipient_users(message_body.recipients)
    )

    is_global = message_body.recipients is None

    sender = get_current_user_object()
    message_folder_path = "messages/tim-messages"

    message_subject = message_body.messageSubject
    timestamp = datetime.now()  # add timestamp to document path to make it unique
    message_path = remove_path_special_chars(f"{timestamp}-{message_subject}")

    check_messages_folder_path("messages", message_folder_path)

    if is_global:
        message_folder_path += "/global"

    message_doc = create_document(
        f"{message_folder_path}/{message_path}",
        message_subject,
        doc_owner=(
            UserGroup.get_admin_group() if is_global else sender.get_personal_group()
        ),
    )

    if recipients:
        message_doc.block.add_rights(recipients, AccessType.view)
    elif is_global:
        message_doc.block.add_rights([UserGroup.get_anonymous_group()], AccessType.view)

    update_tim_msg_doc_settings(
        message_doc, sender if not is_global else None, message_body
    )
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


@tim_message.post("/reply")
def reply_to_tim_message(options: ReplyOptions, message: MessageBody) -> Response:
    message_options = MessageOptions(
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
    if not message.recipients:
        raise RouteException("Reply requires a recipient")

    recipient = User.get_by_name(message.recipients.pop())
    if recipient:
        recipient_email = recipient.email
    else:
        raise NotExist("Recipient not found")

    message = MessageBody(
        message.messageBody, message.messageSubject, [recipient_email]
    )

    return send_message_or_reply(message, message_options)


@tim_message.post("/mark_as_read")
def mark_as_read(message_id: int) -> Response:
    """
    Marks given message as read in database.
    Expects that message receiver and marker are the same person.

    :param message_id: Id of given message
    :return:
    """
    verify_logged_in()

    message = (
        run_sql(select(InternalMessage).filter_by(id=message_id)).scalars().first()
    )
    if not message:
        raise NotExist("Message not found by the ID")
    read_receipt = InternalMessageReadReceipt.get_for_user(
        get_current_user_object(), message
    )
    u = get_current_user_object()
    if read_receipt is None:
        read_receipt = InternalMessageReadReceipt(user=u, message=message)
        db.session.add(read_receipt)

    read_receipt.user = u
    read_receipt.marked_as_read_on = get_current_time()
    db.session.commit()

    return ok_response()


@tim_message.post("/cancel_read_receipt")
def cancel_read_receipt(message_id: int) -> Response:
    """
    Removes read receipt date and the user who marked it from the database entry.

    :param message_id: Message identifier
    :return:
    """
    verify_logged_in()

    receipt = (
        run_sql(
            select(InternalMessageReadReceipt).filter_by(
                user_id=get_current_user_object().id, message_id=message_id
            )
        )
        .scalars()
        .first()
    )
    if not receipt:
        raise NotExist("No read receipt found for the message")
    receipt.marked_as_read_on = None
    db.session.commit()

    return ok_response()


class ReadReceiptFormat(Enum):
    Count = "count"
    CSV = "csv"
    TableFormQuery = "tableform-query"


@tim_message.get("/readReceipts")
def get_read_receipts(
    message_doc: int,
    include_read: bool = False,
    include_unread: bool = False,
    separator: str = ";",
    receipt_format: ReadReceiptFormat = field(
        metadata={"by_value": True}, default=ReadReceiptFormat.CSV
    ),
) -> Response:
    verify_logged_in()
    doc = DocEntry.find_by_id(message_doc)
    if not doc:
        raise NotExist("No document found")
    verify_manage_access(doc)

    read_users = run_sql(
        select(
            InternalMessageReadReceipt.user_id,
            InternalMessageReadReceipt.marked_as_read_on,
            InternalMessageReadReceipt.last_seen,
        )
        .join(InternalMessage)
        .filter(InternalMessage.doc_id == doc.id)
    ).all()

    read_user_map: dict[int, datetime] = {
        user_id: read_time for user_id, read_time, _ in read_users if read_time
    }
    last_seen_user_map: dict[int, datetime] = {
        user_id: last_seen for user_id, _, last_seen in read_users if last_seen
    }

    all_recipients = (
        run_sql(
            select(User)
            .join(UserGroupMember, User.active_memberships)
            .join(
                InternalMessageDisplay,
                InternalMessageDisplay.usergroup_id == UserGroupMember.usergroup_id,
            )
            .join(InternalMessage)
            .filter(InternalMessage.doc_id == doc.id)
        )
        .scalars()
        .all()
    )

    if not all_recipients:
        if include_unread:
            raise RouteException(
                "For performance reasons, only read users can be shown for global messages"
            )
        all_recipients = (
            run_sql(select(User).filter(User.id.in_(read_user_map.keys())))
            .scalars()
            .all()
        )

    if receipt_format == ReadReceiptFormat.Count:
        count_data = [
            ["seen", "read", "all"],
            [
                str(len(last_seen_user_map)),
                str(len(read_user_map)),
                str(len(all_recipients)),
            ],
        ]
        return text_response(csv_string(count_data, "excel", separator))

    data = [["id", "email", "user_name", "real_name", "read_on", "last_seen"]]

    for i, u in enumerate(all_recipients):
        read_time = ""
        last_seen_time = ""
        if u.id in read_user_map:
            if not include_read:
                continue
            read_time = datetime_isoformat(read_user_map[u.id])
        elif not include_unread:
            continue
        if u.id in last_seen_user_map:
            last_seen_time = datetime_isoformat(last_seen_user_map[u.id])
        if not is_hide_names():
            data.append([u.id, u.email, u.name, u.real_name, read_time, last_seen_time])
        else:
            data.append(
                [
                    str(i),
                    f"user_{i}@noreply",
                    f"user{i}",
                    f"User {i}",
                    read_time,
                    last_seen_time,
                ]
            )

    if receipt_format == ReadReceiptFormat.TableFormQuery:
        return text_response("|".join([u[2] for u in data[1:]]))

    return text_response(csv_string(data, "excel", separator))


def get_recipient_users(recipients: list[str] | None) -> list[UserGroup]:
    """
    Finds UserGroup objects of recipients based on their email

    :param recipients: list of recipients' emails
    :return: list of recipient UserGroups
    """
    if not recipients:
        return []
    users = set()
    for rcpt in recipients:
        if not rcpt:
            continue
        if user := User.get_by_email(rcpt):
            users.add(UserGroup.get_by_name(user.name))
        if msg_list := MessageListModel.get_by_email(rcpt):
            stmt = (
                select(UserGroup)
                .join(MessageListTimMember)
                .filter(
                    (MessageListTimMember.message_list_id == msg_list.id)
                    & (MessageListTimMember.membership_ended == None)
                )
            )
            ugs = run_sql(stmt).scalars().all()
            users.update(ugs)

    return list(users)


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
    message_doc: DocInfo, sender: User | None, message_body: MessageBody
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
    if sender:
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
    elif pages and not recipients:
        for page in pages:
            display = InternalMessageDisplay(message=msg, display_block=page.block)
            db.session.add(display)
    elif not pages and recipients:
        for rcpt in recipients:
            display = InternalMessageDisplay(message=msg, usergroup=rcpt)
            db.session.add(display)
    elif not pages and not recipients:
        display = InternalMessageDisplay(message=msg)
        db.session.add(display)
