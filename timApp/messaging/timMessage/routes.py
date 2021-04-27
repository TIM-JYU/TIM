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
        regex = "https?://[a-z.]*/(show_slide|view|teacher|velp|answers|lecture|review|slide)/"
        global shortened_url
        if re.search(regex, url):  # check if url matches the TIM urls' pattern
            shortened_url = re.sub(regex, "", url)
        else:
            shortened_url = url
        document = DocEntry.find_by_path(shortened_url) # check if url exists in TIM
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
        valid_urls = "\n".join(valid_urls)  # turn URL list into a string again
        return json_response({"shortened_urls": valid_urls}, 200)


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
