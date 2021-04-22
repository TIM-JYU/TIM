from dataclasses import dataclass
from typing import Optional, List
from datetime import datetime

from flask import Response

from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.create_item import create_document
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.folder.createopts import FolderCreationOptions
from timApp.folder.folder import Folder
from timApp.messaging.timMessage.internalmessage_models import InternalMessage, DisplayType
from timApp.timdb.sqa import db
from timApp.util.flask.responsehelper import json_response, ok_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import remove_path_special_chars

timMessage = TypedBlueprint('timMessage', __name__, url_prefix='/timMessage')


@dataclass
class MessageOptions:
    # Options regarding TIM messages
    messageChannel: bool
    isPrivate: bool
    archive: bool
    pageList: str
    confirm: bool
    reply: bool
    sender: str
    senderEmail: str
    replyAll: Optional[bool] = None  # VIESTIM: ignore this if !reply
    expires: Optional[datetime] = None


@dataclass
class MessageBody:
    emailbody: str
    emailsubject: str
    recipients: List[str]  # VIESTIM: find recipient by email or some other identifier?


@timMessage.route("/send", methods=['POST'])
def send_tim_message(options: MessageOptions, message: MessageBody) -> Response:
    """
    Creates a new TIM message and saves it to database.

    :param options: Options related to the message
    :param message: Message subject, contents and sender
    :return:
    """
    print(message)

    tim_message = InternalMessage(can_mark_as_read=options.confirm, reply=options.reply)
    create_tim_message(tim_message, message)

    db.session.add(tim_message)
    db.session.commit()

    return ok_response()


def create_tim_message(tim_message: InternalMessage, message_body: MessageBody) -> DocInfo:
    """
    Creates a TIM document for the TIM message to sender's Messages-folder.

    :param tim_message: InternalMessage object
    :param message_body: Message subject, contents and list of recipients
    :return: The created Document object
    """
    sender = get_current_user_object()
    sender_path = sender.get_personal_folder().path

    message_folder_path = f'/{sender_path}/messages'
    if not DocEntry.find_by_path(message_folder_path):  # VIESTIM create this folder somewhere else for all users
        Folder.create(message_folder_path, sender.get_personal_group(),
                      title="Messages",
                      creation_opts=FolderCreationOptions(apply_default_rights=True))

    message_subject = message_body.emailsubject

    message_doc = create_document(f'{message_folder_path}/{remove_path_special_chars(message_subject)}',
                                  message_subject)

    message_doc.document.add_paragraph(f'# {message_subject}')
    message_doc.document.add_paragraph(f'**From:** {sender.name}, {sender.email}')
    message_par = message_doc.document.add_paragraph(message_body.emailbody)

    tim_message.doc_id = message_doc.id
    tim_message.par_id = message_par.get_id()
    tim_message.display_type = DisplayType.TOP_OF_PAGE  # VIESTIM default value for now

    return message_doc


@timMessage.route("/get", methods=['GET'])  # VIESTIM get all messages for testing purposes
def get_tim_messages() -> Response:
    tim_messages: List[InternalMessage] = InternalMessage.get_messages()

    return json_response(tim_messages)
