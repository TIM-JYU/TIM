import uuid
from dataclasses import dataclass, asdict
from datetime import datetime

from flask import Response, request

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import (
    get_current_user_id,
    get_current_user_group,
    get_current_user_object,
)
from timApp.plugin.calendar.models import Event, Eventgroup, Enrollment, Enrollmenttype
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
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


@calendar_plugin.before_app_first_request
def initialize_db():
    """Initializes the enrollment types in the database when the TIM-server is launched the first time,
    before the first request."""

    types = Enrollmenttype.query.filter(
        Enrollmenttype.enroll_type_id == 0
    ).all()  # Remember to add filters here if you add new enrollment types
    if len(types) == 0:
        db.session.add(
            Enrollmenttype(enroll_type_id=0, enroll_type="booking")
        )  # TODO: proper enrollment types
        db.session.commit()


@dataclass
class CalendarItem:
    opiskelijat: str
    ohjaajat: str

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class CalendarMarkup(GenericMarkupModel):
    ryhmat: list[CalendarItem] | None = None


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
    """Fetches the user's events and the events that have a relation to user's groups from the database in JSON or
    ICS format, specified in the query-parameter

    :return: User's events in JSON or ICS format or HTTP 400 if failed
    """
    verify_logged_in()
    cur_user = get_current_user_id()

    events: list[Event] = Event.query.filter(Event.creator_user_id == cur_user).all()

    user_obj = get_current_user_object()

    for group in user_obj.groups:
        group_events = Eventgroup.query.filter(
            Eventgroup.usergroup_id == group.id
        ).all()
        for group_event in group_events:
            event = Event.get_event_by_id(group_event.event_id)
            if event not in events:
                events.append(event)
            elif event is None:
                print("Event not found by the id of", group_event.event_id)

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
            for event in events:
                enrollments = len(Event.get_event_by_id(event.event_id).enrolled_users)
                event_objs.append(
                    {
                        "id": event.event_id,
                        "title": event.title,
                        "start": event.start_time,
                        "end": event.end_time,
                        "meta": {"enrollments": enrollments, "maxSize": event.max_size},
                    }
                )
            return json_response(event_objs)
    raise RouteException("Unsupported file type")


@dataclass
class CalendarEvent:
    title: str
    start: datetime
    end: datetime
    event_groups: list[str] | None
    max_size: int


@calendar_plugin.post("/events")
def add_events(events: list[CalendarEvent]) -> Response:
    """Persists the given list of events to the database

    :param events: List of events to be persisted
    :return: Persisted events in JSON with updated ids
    """
    verify_logged_in()
    # TODO: use get_current_user_object() to access more user information, e.g. user's groups
    cur_user = get_current_user_id()
    added_events = []
    for event in events:
        groups = []
        for event_group in event.event_groups:
            groups.append(UserGroup.get_by_name(event_group))

        event = Event(
            title=event.title,
            start_time=event.start,
            end_time=event.end,
            creator_user_id=cur_user,
            groups_in_event=groups,
            max_size=event.max_size,
        )
        db.session.add(event)
        added_events.append(event)

    db.session.commit()
    # To fetch the new ids generated by the database
    event_list = []
    for event in added_events:
        event_list.append(
            {
                "id": event.event_id,
                "title": event.title,
                "start": event.start_time,
                "end": event.end_time,
                "meta": {
                    "enrollments": 0,
                    "maxSize": event.max_size,
                },
            }
        )

    return json_response(event_list)


@calendar_plugin.put("/events/<int:event_id>")
def edit_event(event_id: int, event: CalendarEvent) -> Response:
    """Edits the event by the given id with the given event

    :param event_id: Event id
    :param event: Updated event
    :return: HTTP 200 if succeeded, otherwise 400
    """
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
    """Deletes the event by the given id

    :param event_id: Event id
    :return: HTTP 200 if succeeded, otherwise 400
    """
    verify_logged_in()
    event = Event.get_event_by_id(event_id)
    if not event:
        raise RouteException("Event not found")
    db.session.delete(event)
    db.session.commit()
    return ok_response()


@calendar_plugin.post("/bookings")
def book_event(event_id: int) -> Response:
    verify_logged_in()
    # user_id = get_current_user_id()
    user_obj = get_current_user_object()
    # user_group_ids = UserGroupMember.query.filter(
    #    UserGroupMember.user_id == user_id
    # ).all()

    group_id = None
    for group in user_obj.groups:
        if group.name == user_obj.name:
            group_id = group.id

    enrollment = Enrollment(
        event_id=event_id, usergroup_id=group_id, enroll_type_id=0
    )  # TODO: add enrollment types

    db.session.add(enrollment)
    db.session.commit()

    return ok_response()


register_html_routes(calendar_plugin, class_schema(CalendarHtmlModel), reqs_handle)
