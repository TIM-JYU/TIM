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
from timApp.plugin.calendar.models import Event, ExportedCalendar
from timApp.tim_app import app
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
    domain = app.config["TIM_HOST"]
    url = domain + "/calendar/ical?user="
    cur_user = get_current_user_id()
    user_data: ExportedCalendar = ExportedCalendar.query.filter(
        ExportedCalendar.user_id == cur_user
    ).one_or_none()
    if user_data is not None:
        url = url + user_data.calendar_hash
        return text_response(url)
    hash_code = secrets.token_urlsafe(16)
    user_data = ExportedCalendar(
        user_id=cur_user,
        calendar_hash=hash_code,
    )
    db.session.add(user_data)
    db.session.commit()
    url = url + hash_code
    return text_response(url)


@calendar_plugin.get("/ical")
def get_ical(user: str) -> Response:
    user_data: ExportedCalendar = ExportedCalendar.query.filter(
        ExportedCalendar.calendar_hash == user
    ).one_or_none()
    user_id = user_data.user_id
    events: list[Event] = Event.query.filter(Event.creator_user_id == user_id).all()
    buf = StringIO()
    buf.write("BEGIN:VCALENDAR\r\n")
    buf.write("PRODID:-//TIM Katti-kalenteri//iCal4j 1.0//EN\r\n")
    buf.write("VERSION:2.0\r\n")
    buf.write("CALSCALE:GREGORIAN\r\n")
    for event in events:
        dts = event.start_time.strftime("%Y%m%dT%H%M%S")
        dtend = event.end_time.strftime("%Y%m%dT%H%M%S")

        buf.write("BEGIN:VEVENT\r\n")
        buf.write("DTSTART:" + dts + "Z\r\n")
        buf.write("DTEND:" + dtend + "Z\r\n")
        buf.write("DTSTAMP:" + dts + "Z\r\n")
        buf.write("UID:" + uuid.uuid4().hex[:9] + "@tim.jyu.fi\r\n")
        buf.write("CREATED:" + dts + "Z\r\n")
        buf.write("SUMMARY:" + event.title + "\r\n")
        buf.write("END:VEVENT\r\n")

    buf.write("END:VCALENDAR\r\n")
    result = buf.getvalue()
    return Response(result, mimetype="text/calendar")


@calendar_plugin.get("/events")
def get_events() -> Response:
    verify_logged_in()
    cur_user = get_current_user_id()
    events: list[Event] = Event.query.filter(Event.creator_user_id == cur_user).all()
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
