import json
import tempfile, os
import uuid
from dataclasses import dataclass, asdict
from datetime import datetime
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


@calendar_plugin.get("/events")
def get_events() -> Response:
    verify_logged_in()
    cur_user = get_current_user_id()
    events: list[Event] = Event.query.filter(Event.creator_user_id == cur_user).all()

    file_type = request.args.get("file_type")
    match file_type:
        case "ics":
            ics_file = ""
            ics_file = ics_file + "BEGIN:VCALENDAR\n"
            ics_file = ics_file + "PRODID:-//TIM Katti-kalenteri//iCal4j 1.0//EN\n"
            ics_file = ics_file + "VERSION:2.0\n"
            ics_file = ics_file + "CALSCALE:GREGORIAN\n"
            for event in events:
                dts = event.start_time.strftime("%Y%m%dT%H%M%S")
                dtend = event.end_time.strftime("%Y%m%dT%H%M%S")

                ics_file = ics_file + "BEGIN:VEVENT\n"
                ics_file = ics_file + "DTSTART:" + dts + "Z\n"
                ics_file = ics_file + "DTEND:" + dtend + "Z\n"
                ics_file = ics_file + "DTSTAMP:" + dts + "Z\n"
                ics_file = ics_file + "UID:" + uuid.uuid4().hex[:9] + "@tim.jyu.fi\n"
                ics_file = ics_file + "CREATED:" + dts + "Z\n"
                ics_file = ics_file + "SUMMARY:" + event.title + "\n"
                ics_file = ics_file + "END:VEVENT\n"

            ics_file = ics_file + "END:VCALENDAR\n"
            return text_response(ics_file)
        case "json":
            event_objs = []
            for i, event in enumerate(events):
                event_objs.append(
                    {
                        "id": i,
                        "title": event.title,
                        "start": event.start_time,
                        "end": event.end_time,
                    }
                )
            return json_response(event_objs)
    raise RouteException("Unsupported file type")


@dataclass
class CalendarEvent:
    id: Any
    meta: Any
    actions: Any
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
