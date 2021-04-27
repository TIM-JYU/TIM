import re
from dataclasses import dataclass
from typing import Optional, List
from datetime import datetime

from flask import Response

from timApp.auth.sessioninfo import get_current_user_object
from timApp.auth.accesshelper import verify_edit_access
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
# VIESTIM: change this to take List[str] as argument
def check_urls(urls: str) -> Response:
    print("tsekataan: " + urls)
    regex = "https?://[a-z.]*/(show_slide|view|teacher|velp|answers|lecture|review|slide)/"
    global shortened_url
    if re.search(regex, urls):
        print("oikean muotoinen url")
        shortened_url = re.sub(regex, "", urls)
        print(shortened_url)
    else:
        return json_response({"error": "URL not found"}, 404)
    document = DocEntry.find_by_path(shortened_url)
    print("document: " + str(document))
    if document is None:
        return json_response({"error": "URL not found"}, 404)
    try:
        access = verify_edit_access(document)
        print("access: " + str(access))
        return json_response({"shortened_url": shortened_url}, 200)
    except Exception:
        return json_response({"error": "You don't have permission to post TIM message to this page"}, 401)

@timMessage.route("/send", methods=['POST'])
def send_tim_message(options: MessageOptions, message: MessageBody) -> Response:
    """
    Creates a new TIM message and saves it to database.

    :param options: Options related to the message
    :param message: Message subject, contents and sender
    :return:
    """
    print(message)

    tim_message = InternalMessage(can_mark_as_read=options.readReceipt, reply=options.reply)
    create_tim_message(tim_message, options, message)

    db.session.add(tim_message)
    db.session.commit()

    return ok_response()


def create_tim_message(tim_message: InternalMessage, options: MessageOptions, message_body: MessageBody) -> DocInfo:
    """
    Creates a TIM document for the TIM message to sender's Messages-folder.

    :param tim_message: InternalMessage object
    :param options: Options related to the message
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

    message_subject = message_body.messageSubject

    message_doc = create_document(f'{message_folder_path}/{remove_path_special_chars(message_subject)}',
                                  message_subject)

    message_doc.document.add_paragraph(f'# {message_subject}')
    message_doc.document.add_paragraph(f'**From:** {sender.name}, {sender.email}')
    message_doc.document.add_paragraph(f'**To:** {message_body.recipients}')
    message_par = message_doc.document.add_paragraph(message_body.messageBody)

    tim_message.doc_id = message_doc.id
    tim_message.par_id = message_par.get_id()
    if options.important:
        # Important messages are interpreted to 'sticky'
        tim_message.display_type = DisplayType.STICKY  # TODO actual functionality
    else:
        tim_message.display_type = DisplayType.TOP_OF_PAGE  # default display type

    return message_doc


@timMessage.route("/get", methods=['GET'])  # VIESTIM get all messages for testing purposes
def get_tim_messages() -> Response:
    tim_messages: List[InternalMessage] = InternalMessage.get_messages()

    return json_response(tim_messages)
