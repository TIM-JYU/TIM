import json
import secrets
import tempfile, os
import socket
from urllib.parse import urlparse
import uuid
from dataclasses import dataclass, asdict
from datetime import datetime
from io import StringIO
from typing import Any

from flask import Response, request

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_id, get_current_user_object
from timApp.plugin.calendar.models import Event
from timApp.timdb.sqa import db
from timApp.util.flask.requesthelper import RouteException


from timApp.util.flask.responsehelper import json_response, ok_response, text_response

from timApp.util.flask.typedblueprint import TypedBlueprint
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
)

calendar_plugin = TypedBlueprint("calendar_plugin", __name__, url_prefix="/calendar")


@dataclass
class CalendarItem:
    done: bool
    text: str

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class CalendarMarkup(GenericMarkupModel):
    todos: list[CalendarItem] | None = None


@dataclass
class CalendarStateModel:
    pass


@dataclass
class CalendarInputModel:
    pass


@dataclass
class CalendarHtmlModel(
    GenericHtmlModel[CalendarInputModel, CalendarMarkup, CalendarStateModel]
):
    def get_component_html_name(self) -> str:
        return "tim-calendar"

    def get_static_html(self) -> str:
        return """
            <div>Calendar</div>
        """


def reqs_handle() -> PluginReqs:
    return {"js": ["calendar"], "multihtml": True}


@calendar_plugin.get("/")
def get_todos() -> Response:
    # user = get_current_user_object()
    # user.
    return json_response(
        {
            "todos": [
                "asd",
                "wer",
                "rtq",
            ]
        }
    )


@calendar_plugin.get("/export")
def get_url() -> Response:
    verify_logged_in()
    cur_user = get_current_user_id()
    hash_code = secrets.token_urlsafe(16)
    # TODO add user_id and hash_code to database
    current_path = request.full_path
    o = urlparse(current_path)
    domain = o.hostname
    if domain == None:
        domain = ""
    url = domain + "/calendar/events?user=" + hash_code
    return text_response(url)


@calendar_plugin.get("/events")
def get_events(file_type: str = "json") -> Response:
    verify_logged_in()
    cur_user = get_current_user_id()
    events: list[Event] = Event.query.filter(Event.creator_user_id == cur_user).all()

    match file_type:
        case "ics":
            buf = StringIO()
            buf.write("BEGIN:VCALENDAR\n")
            buf.write("PRODID:-//TIM Katti-kalenteri//iCal4j 1.0//EN\n")
            buf.write("VERSION:2.0\n")
            buf.write("CALSCALE:GREGORIAN\n")
            for event in events:
                dts = event.start_time.strftime("%Y%m%dT%H%M%S")
                dtend = event.end_time.strftime("%Y%m%dT%H%M%S")

                buf.write("BEGIN:VEVENT\n")
                buf.write("DTSTART:" + dts + "Z\n")
                buf.write("DTEND:" + dtend + "Z\n")
                buf.write("DTSTAMP:" + dts + "Z\n")
                buf.write("UID:" + uuid.uuid4().hex[:9] + "@tim.jyu.fi\n")
                buf.write("CREATED:" + dts + "Z\n")
                buf.write("SUMMARY:" + event.title + "\n")
                buf.write("END:VEVENT\n")

            buf.write("END:VCALENDAR\n")
            result = buf.getvalue()
            return text_response(result)
        case "json":
            event_objs = []
            for event in events:
                event_objs.append(
                    {
                        "id": event.event_id,
                        "title": event.title,
                        "start": event.start_time,
                        "end": event.end_time,
                    }
                )
            return json_response(event_objs)
    raise RouteException("Unsupported file type")


@dataclass
class CalendarEvent:
    title: str
    start: datetime
    end: datetime


@calendar_plugin.post("/events")
def add_events(events: list[CalendarEvent]) -> Response:
    verify_logged_in()
    # TODO: use get_current_user_object() to access more user information, e.g. user's groups
    cur_user = get_current_user_id()

    for event in events:
        event = Event(
            title=event.title,
            start_time=event.start,
            end_time=event.end,
            creator_user_id=cur_user,
        )
        db.session.add(event)

    db.session.commit()
    return json_response(events)


@calendar_plugin.put("/events/<int:event_id>")
def edit_event(event_id: int, event: CalendarEvent) -> Response:
    verify_logged_in()
    old_event = Event.get_event_by_id(event_id)
    if not old_event:
        raise RouteException("Event not found")
    old_event.title = event.title
    old_event.start_time = event.start
    old_event.end_time = event.end
    db.session.commit()
    return ok_response()


@calendar_plugin.delete("/events/<int:event_id>")
def delete_event(event_id: int) -> Response:
    verify_logged_in()
    event = Event.get_event_by_id(event_id)
    if not event:
        raise RouteException("Event not found")
    db.session.delete(event)
    db.session.commit()
    return ok_response()


register_html_routes(calendar_plugin, class_schema(CalendarHtmlModel), reqs_handle)
