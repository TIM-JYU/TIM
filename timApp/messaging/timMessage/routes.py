from dataclasses import dataclass
from typing import Optional
from datetime import datetime, timezone

from flask import Response

from timApp.util.flask.responsehelper import ok_response, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

timMessage = TypedBlueprint('timMessage', __name__, url_prefix='/timMessage')

@dataclass
class MessageOptions:
    #Options regarding TIM messages
    rcpt: str #multiple email addresses separated by ; VIESTIM: find recipient by email or some other identifier?
    emailsubject: str
    emailbody: str
    messageChannel: bool
    private: bool
    archive: bool
    pageList: str
    check: bool
    reply: bool
    replyAll: bool
    expires: Optional[datetime] = None
    sender: Optional[str] = None #VIESTIM: can't send message without sender!
    senderEmail: Optional[str] = None #VIESTIM: can't send message without email?

@timMessage.route("/send", methods=['POST'])
def send_tim_message(options: MessageOptions) -> Response:
    #TODO: actual logic, this is just a placeholder
    print(options)
    return ok_response()