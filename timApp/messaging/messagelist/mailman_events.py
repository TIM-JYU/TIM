# Mailman event handler
# Listens to events sent by https://github.com/dezhidki/mailman-rest-events

from dataclasses import dataclass
from typing import Optional

from flask import request, Response, Blueprint

from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.messaging.messagelist.messagelist_utils import parse_mailman_message, archive_message
from timApp.tim_app import app, csrf
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import ok_response
from timApp.util.logger import log_warning
from tim_common.marshmallow_dataclass import class_schema

mailman_events = Blueprint("mailman_events", __name__, url_prefix="/mailman/event")

AUTH_USER = app.config.get("MAILMAN_EVENT_API_USER", None)
AUTH_KEY = app.config.get("MAILMAN_EVENT_API_KEY", None)


def has_valid_event_auth() -> bool:
    return AUTH_USER is not None and AUTH_KEY is not None


def check_auth() -> bool:
    auth = request.authorization
    return auth is not None and has_valid_event_auth() \
           and auth.type == "basic" and auth.username == AUTH_USER and auth.password == AUTH_KEY


@dataclass
class MailmanMessageList:
    id: str
    name: str
    host: str


@dataclass
class MailmanMemberAddress:
    email: str
    name: Optional[str]  # Names associated with an (member) email addresses are optional in Mailman.


@dataclass
class MailmanMember:
    user_id: int
    address: MailmanMemberAddress


@dataclass
class SubscriptionEvent:
    event: str
    mlist: MailmanMessageList
    member: MailmanMember


SubscriptionEventSchema = class_schema(SubscriptionEvent)


@dataclass
class NewMessageEvent:
    event: str
    mlist: MailmanMessageList
    message: dict


NewMessageEventSchema = class_schema(NewMessageEvent)

EVENTS = {
    "user_subscribed": SubscriptionEventSchema(),
    "user_unsubscribed": SubscriptionEventSchema(),
    "new_message": NewMessageEventSchema()
}


@mailman_events.post("")
@csrf.exempt
def handle_event() -> Response:
    """Handle events sent by Mailman."""
    if not check_auth():
        return Response(status=401, headers={"WWW-Authenticate": "Basic realm=\"Needs auth\""})

    if not request.is_json:
        raise RouteException("Body must be JSON")

    data = request.json
    if not data or not isinstance(data, dict):
        raise RouteException("Body must be JSON object")

    if "event" not in data or data["event"] not in EVENTS:
        raise RouteException("Event not handled")

    evt = EVENTS[data["event"]].load(data)

    if isinstance(evt, SubscriptionEvent):
        if evt.event == "user_subscribed":
            # TODO: Handle subscription event.
            pass
        elif evt.event == "user_unsubscribed":
            # TODO: Handle unsubscription event.
            pass
    # TODO: Check if this message is a duplicate. If it is, then handle (e.g. drop) it. How to check if the message
    #  is a duplicate? If we are checking for a duplicate, should we be counting how "manyeth" duplicate the message
    #  is, so we can e.g. catch if there is a spammer channel that bombards with duplicate messages?
    elif isinstance(evt, NewMessageEvent):
        handle_new_message(evt)

    return ok_response()


def handle_new_message(event: NewMessageEvent) -> None:
    """Handles an event raised by a new message.

    :param event: Contains information about a new message sent to Mailman's list.
    """
    m_list_name, _, _ = event.mlist.name.partition("@")
    message_list = MessageListModel.get_by_name(m_list_name)

    if message_list is None:
        raise RouteException("Message list does not exist.")
    if not message_list.email_list_domain == event.mlist.host:
        # If we are here, something is now funky. Message list doesn't have a email list (domain) configured,
        # but messages are directed at it. Not sure what do exactly do here, honestly, except log the event for
        # further investigation.
        log_warning(f"Message list '{message_list.name}' with id '{message_list.id}' doesn't have a domain "
                    f"configured properly. Domain '{event.mlist.host}' was expected.")
        raise RouteException("List not configured properly.")
    parsed_message = parse_mailman_message(event.message, message_list)
    archive_message(message_list, parsed_message)
    # TODO: Relay this message forward, if there are other message channels in use for a message list.
