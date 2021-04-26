from dataclasses import dataclass
from typing import Optional, List
from datetime import datetime

from flask import Response

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import create_document
from timApp.document.docinfo import DocInfo
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.messaging.timMessage.internalmessage_models import InternalMessage, DisplayType
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.flask.responsehelper import json_response, ok_response
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
    timestamp = datetime.now()
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

    return message_doc


@timMessage.route("/reply", methods=['POST'])
def reply_to_tim_message(options: MessageOptions, message: MessageBody) -> Response:
    # TODO handle replying to message
    return ok_response()


@timMessage.route("/get", methods=['GET'])  # VIESTIM get all messages for testing purposes
def get_tim_messages() -> Response:
    tim_messages: List[InternalMessage] = InternalMessage.get_messages()

    return json_response(tim_messages)


def get_recipient_users(recipients: List[str]) -> List[UserGroup]:
    """
    Finds UserGroup objects of recipients based on their email

    :param recipients: list of recipients' emails
    :return: list of recipient UserGroups
    """
    users = []
    for rcpt in recipients:
        users.append(UserGroup.get_by_name(User.get_by_email(rcpt).name))

    return users


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
        msg_folder.block.add_rights([UserGroup.get_logged_in_group()], AccessType.view)

    tim_msg_folder = Folder.find_by_location(tim_msg_folder_path, 'tim-messages')

    if not tim_msg_folder:
        tim_msg_folder = Folder.create(tim_msg_folder_path, UserGroup.get_admin_group(), title='TIM messages',
                                       creation_opts=FolderCreationOptions(apply_default_rights=True))
        tim_msg_folder.block.add_rights([UserGroup.get_logged_in_group()], AccessType.view)

    return tim_msg_folder
