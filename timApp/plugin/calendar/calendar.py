import secrets
import uuid
from dataclasses import dataclass, asdict
from datetime import datetime
from io import StringIO

from flask import Response

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import (
    get_current_user_id,
    get_current_user_object,
)
from timApp.plugin.calendar.models import Event, Eventgroup, Enrollment, Enrollmenttype
from timApp.plugin.calendar.models import ExportedCalendar
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
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
def initialize_db() -> None:
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


@calendar_plugin.get("/export")
def get_url() -> Response:
    """Creates a unique URl for user to be used when calendar is exported. User ID is
    bind to specific hash code

    :return: URL with hash code
    """
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
    """Fetches users events in a ICS format. User ID is sorted out from hash code from query parameter

    :return: ICS file that can be exported
    """
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
    """Fetches the user's events and the events that have a relation to user's groups from the database in JSON
    format

    :return: User's events in JSON format or HTTP 400 if failed
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
            if event is not None:
                event_obj: Event = event
                if event_obj not in events:
                    events.append(event_obj)
            else:
                print("Event not found by the id of", group_event.event_id)

    event_objs = []
    for event in events:
        event_optional = Event.get_event_by_id(event.event_id)
        if event_optional is not None:
            event_obj = event_optional
            enrollments = len(event_obj.enrolled_users)
            booker_groups = event_obj.enrolled_users
            groups = []
            for group in booker_groups:
                users = []
                for user in group.users:
                    users.append(
                        {
                            "id": user.id,
                            "name": user.real_name,
                            "email": user.email,
                        }
                    )
                groups.append({"name": group.name, "users": users})
            event_objs.append(
                {
                    "id": event_obj.event_id,
                    "title": event_obj.title,
                    "start": event_obj.start_time,
                    "end": event_obj.end_time,
                    "meta": {
                        "enrollments": enrollments,
                        "maxSize": event_obj.max_size,
                        "booker_groups": groups,
                    },
                }
            )
        else:
            print(
                "Error fetching the event by the id of", event.event_id
            )  # should be never possible

    return json_response(event_objs)


@dataclass
class CalendarEvent:
    title: str
    start: datetime
    end: datetime
    max_size: int = 1
    event_groups: list[str] | None = None


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
        group_names = event.event_groups
        if group_names is not None:
            group_name_strs: list[str] = group_names
            for event_group in group_name_strs:
                group = UserGroup.get_by_name(event_group)
                if group is not None:
                    groups.append(group)

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
    """
    Books the event for current user's personal user group.
    TODO: implement booking for user's other groups

    :param event_id: Event id
    :return: HTTP 200 if succeeded, 400 if the event is already full
    """
    verify_logged_in()
    event = Event.get_event_by_id(event_id)
    if event is not None:
        event_obj: Event = event
        if len(event_obj.enrolled_users) >= event_obj.max_size:
            raise RouteException("Event is already full")
    else:
        raise RouteException(f"Event not found by the id of {0}".format(event_id))
    user_obj = get_current_user_object()

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


@calendar_plugin.delete("/bookings/<int:event_id>")
def delete_booking(event_id: int) -> Response:
    """
    Deletes the booking or enrollment to an event for current user's personal user group.

    :param event_id: Event id that matches with the enrollment
    :return: HTTP 200 if succeeded, otherwise 400
    """
    verify_logged_in()
    user_obj = get_current_user_object()
    group_id = -1
    for group in user_obj.groups:
        if group.name == user_obj.name:
            group_id = group.id

    enrollment = Enrollment.get_enrollment_by_ids(event_id, group_id)
    if not enrollment:
        raise RouteException("Enrollment not found")

    db.session.delete(enrollment)
    db.session.commit()
    return ok_response()


register_html_routes(calendar_plugin, class_schema(CalendarHtmlModel), reqs_handle)
