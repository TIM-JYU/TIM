# Mailman event handler
# Listens to events sent by https://github.com/dezhidki/mailman-rest-events

import secrets
from dataclasses import dataclass

from flask import request, Response, Blueprint

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
    if not check_auth():
        return Response(status=401, headers={"WWW-Authenticate": "Basic realm=\"Needs auth\""})

    if not request.is_json:
        raise RouteException("Body must be JSON")

    data = request.json
    if "event" not in data or data["event"] not in EVENTS:
        raise RouteException("Event not handled")
    evt = EVENTS[data["event"]].load(data)

    log_info(f"Got event: {evt}")
    return ok_response()
