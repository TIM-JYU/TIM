# Mailman event handler
# Listens to events sent by https://github.com/dezhidki/mailman-rest-events

import secrets
from dataclasses import dataclass

from flask import request, Response, Blueprint

from timApp.messaging.messagelist.messagelist_models import MessageListModel
from timApp.messaging.messagelist.messagelist_utils import parse_mailman_message, archive_message
from timApp.tim_app import app, csrf
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import ok_response
from timApp.util.logger import log_warning, log_info
from tim_common.marshmallow_dataclass import class_schema

mailman_events = Blueprint("mailman_events", __name__, url_prefix="/mailman/event")

AUTH_USER = app.config.get("MAILMAN_EVENT_API_USER") or ""
AUTH_KEY = app.config.get("MAILMAN_EVENT_API_KEY") or ""
if not AUTH_USER or not AUTH_KEY:
    log_warning("No mailman event API credentials set! Generating random credentials.")
    AUTH_USER = secrets.token_urlsafe(32)
    AUTH_KEY = secrets.token_urlsafe(32)


def check_auth() -> bool:
    auth = request.authorization
    return auth is not None and auth.type == "basic" and auth.username == AUTH_USER and auth.password == AUTH_KEY


@dataclass
class MailmanMessageList:
    id: str
    name: str
    host: str


@dataclass
class MailmanMemberAddress:
    email: str
    name: str


@dataclass
class MailmanMember:
    user_id: str
    name: str


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


@mailman_events.route("", methods=["POST"])
@csrf.exempt
def handle_event() -> Response:
    """Handle events send by Mailman."""
    if not check_auth():
        return Response(status=401, headers={"WWW-Authenticate": "Basic realm=\"Needs auth\""})

    if not request.is_json:
        raise RouteException("Body must be JSON")

    data = request.json
    if "event" not in data or data["event"] not in EVENTS:
        # VIESTIM: Should we log if an unhandled event happened?
        #  e.g. log_error("A call without any event field has occured.")
        #  and/or log_error(f"Unhandled event {data['event']}.")
        raise RouteException("Event not handled")
    evt = EVENTS[data["event"]].load(data)

    log_info(f"Got event: {evt}")
    if isinstance(evt, SubscriptionEvent):
        if evt.event == "user_subscribed":
            # TODO: Handle subscription event.
            pass
        elif evt.event == "user_unsubscribed":
            # TODO: Handle unsubscription event.
            pass
    # TODO: Check if this message is a duplicate. If it is, then handle (e.g. drop) it.
    # VIESTIM: How to check if the message is a duplicate?
    # VIESTIM: If we are checking for a duplicate, should we be counting how "manyeth" duplicate the message is, so
    #  we can e.g. catch if there is a spammer channel that bombards with duplicate messages?
    # TODO: Archive this message, if it is intended for a message list that has archiving on.
    elif isinstance(evt, NewMessageEvent):
        message_list = MessageListModel.get_list_by_name_first(evt.mlist.name)
        if message_list is None:
            raise RouteException("Message list does not exist.")
        if not message_list.email_list_domain == evt.mlist.host:
            # VIESTIM: If we are here, something is now funky. Message list doesn't have a email list (domain)
            #  configured, but messages are directed at it. Not sure what do exactly do here, honestly.
            log_warning(f"Message list '{message_list.name}' with id '{message_list.id}' doesn't have a domain "
                        f"configured properly. Domain '{evt.mlist.host}' was expected.")
        parsed_message = parse_mailman_message(evt.message)
        archive_message(parsed_message)
        # TODO: Relay this message forward, if there are other message channels in use for a message list.

    return ok_response()
