"""
API endpoints for the calendar plugin
"""
import secrets
import uuid
from dataclasses import dataclass, asdict, field
from datetime import datetime
from io import StringIO
from textwrap import wrap

from flask import Response, render_template_string, url_for
from werkzeug.exceptions import NotFound

from timApp.auth.accesshelper import verify_logged_in, AccessDenied
from timApp.auth.sessioninfo import (
    get_current_user_id,
    get_current_user_object,
)
from timApp.notification.send_email import send_email
from timApp.plugin.calendar.models import Event, EventGroup, Enrollment
from timApp.plugin.calendar.models import ExportedCalendar
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_access
from timApp.user.user import User, manage_access_set, edit_access_set
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response, text_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import fin_timezone
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
)

calendar_plugin = TypedBlueprint("calendar_plugin", __name__, url_prefix="/calendar")


@dataclass
class FilterOptions:
    """Calendar markup fields for filtering options"""

    groups: list[str] | None = None
    tags: list[str] | None = None
    fromDate: datetime | None = None
    toDate: datetime | None = None

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class EventTemplate:
    """Calendar markup fields for event template"""

    title: str | None = None
    bookers: list[str] = field(default_factory=list)
    setters: list[str] = field(default_factory=list)
    capacity: int = 0
    tags: list[str] = field(default_factory=list)

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class CalendarMarkup(GenericMarkupModel):
    """Highest level attributes in the calendar markup"""

    filter: FilterOptions = field(default_factory=FilterOptions)
    eventTemplates: dict[str, EventTemplate] = field(default_factory=dict)


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


@calendar_plugin.get("/export")
def get_url() -> Response:
    """Creates a unique URl for user to be used when calendar is exported. User ID is
    bind to specific hash code

    :return: URL with hash code
    """
    verify_logged_in()
    cur_user = get_current_user_id()
    user_data: ExportedCalendar = ExportedCalendar.query.filter(
        ExportedCalendar.user_id == cur_user
    ).one_or_none()
    if user_data is not None:
        hash_code = user_data.calendar_hash
        url = url_for("calendar_plugin.get_ical", key=hash_code, _external=True)
        return text_response(url)
    hash_code = secrets.token_urlsafe(16)
    user_data = ExportedCalendar(
        user_id=cur_user,
        calendar_hash=hash_code,
    )
    db.session.add(user_data)
    db.session.commit()
    url = url_for("calendar_plugin.get_ical", key=hash_code, _external=True)
    return text_response(url)


@calendar_plugin.get("/ical")
def get_ical(key: str) -> Response:
    """Fetches users events in a ICS format. User ID is sorted out from hash code from query parameter

    :return: ICS file that can be exported
    """
    user_data: ExportedCalendar = ExportedCalendar.query.filter(
        ExportedCalendar.calendar_hash == key
    ).one_or_none()
    if user_data is None:
        raise NotFound()

    user_obj = user_data.user
    events = events_of_user(user_obj)

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
        if event.title is None:
            event.title = ""
        buf.write("SUMMARY:" + string_to_lines(event.title) + "\r\n")
        if event.location is None:
            event.location = ""
        buf.write("LOCATION:" + string_to_lines(event.location) + "\r\n")
        if event.message is None:
            event.message = ""
        buf.write("DESCRIPTION:" + string_to_lines(event.message) + "\r\n")
        buf.write("END:VEVENT\r\n")

    buf.write("END:VCALENDAR\r\n")
    result = buf.getvalue()
    return Response(result, mimetype="text/calendar")


def string_to_lines(str_to_split: str) -> str:
    """Splits long strings to n char lines

    :return: str where lines are separated with \r\n + whitespace
    """
    return "\r\n ".join(wrap(str_to_split, width=60))


def events_of_user(user_obj: User) -> list[Event]:
    """
    Fetches users events

    :param: user_obj current user object: User
    :return: list of events
    """
    cur_user = user_obj.id
    events: list[Event] = Event.query.filter(Event.creator_user_id == cur_user).all()
    # TODO: Makes too many queries to db, to be refactored
    for group in user_obj.groups:
        group_events = EventGroup.query.filter(
            EventGroup.usergroup_id == group.id
        ).all()
        for group_event in group_events:
            event = Event.get_event_by_id(group_event.event_id)
            if event is not None:
                event_obj: Event = event
                if event_obj not in events:
                    events.append(event_obj)
            else:
                no_event_found = f"Event not found by the id of {group_event.event_id}"
                raise NotExist(no_event_found)
    return events


@calendar_plugin.get("/events")
def get_events() -> Response:
    """Fetches the events created by the user and the events that have a relation to user's groups from the database
    in JSON format

    :return: User's events in JSON format or HTTP 400 if failed
    """
    verify_logged_in()
    cur_user = get_current_user_id()
    user_obj = get_current_user_object()

    events = events_of_user(user_obj)

    event_objs = []
    for event in events:
        enrollment_amount = len(event.enrolled_users)
        booker_groups = event.enrolled_users
        groups = []

        for group in booker_groups:
            enrollment = Enrollment.get_enrollment_by_ids(event.event_id, group.id)
            if enrollment is not None:
                book_msg = enrollment.booker_message
            else:
                book_msg = ""
            users = []
            cur_user_booking = False
            for user in group.users:
                if user.id == cur_user:
                    cur_user_booking = True
                users.append(
                    {
                        "id": user.id,
                        "name": user.real_name,
                        "email": user.email,
                    }
                )

            groups.append(
                {
                    "name": group.name,
                    "message": book_msg,
                    "users": users,
                }
            )

            if user_is_event_manager(event.event_id) or cur_user_booking:
                groups.append({"name": group.name, "users": users})

        event_objs.append(
            {
                "id": event.event_id,
                "title": event.title,
                "start": event.start_time,
                "end": event.end_time,
                "meta": {
                    "description": event.message,
                    "enrollments": enrollment_amount,
                    "maxSize": event.max_size,
                    "location": event.location,
                    "booker_groups": groups,
                    "signup_before": event.signup_before,
                },
            }
        )

    return json_response(event_objs)


@calendar_plugin.get("/events/<int:event_id>/bookers")
def get_event_bookers(event_id: int) -> str | Response:
    """Fetches all enrollments from the database for the given event and returns the full name and email of every
    booker in a html table

    :param event_id: event id
    :return: Full name and email of every booker of the given event in a html table"""

    verify_logged_in()
    if not user_is_event_manager(event_id):
        raise AccessDenied("No permission to see event bookers")

    event = Event.get_event_by_id(event_id)
    if event is None:
        no_event_found = f"Event not found by the id of {event_id}"
        raise NotExist(no_event_found)

    bookers_info = []
    booker_groups = event.enrolled_users
    for booker_group in booker_groups:
        bookers = booker_group.users
        for booker in bookers:
            bookers_info.append({"full_name": booker.real_name, "email": booker.email})

    return render_template_string(
        """
    <style>
               table, th, td {
                 border: 1px solid black;
               }
    </style>
    <body>
        <table>
            <tr>
                <th>Full name</th>
                <th>Email</th>
            </tr>
            {% for booker in bookers_info %}
                <tr>
                    <td>{{ booker.full_name }}</td>
                    <td>{{ booker.email }}</td>
                </tr>
            {% endfor %}
        </table>
    </body>
    """,
        bookers_info=bookers_info,
    )


@dataclass
class CalendarEvent:
    title: str
    start: datetime
    end: datetime
    signup_before: datetime
    max_size: int
    location: str
    description: str
    booker_groups: list[str] | None = None
    setter_groups: list[str] | None = None
    event_groups: list[str] | None = None
    id: int = -1


@calendar_plugin.post("/events")
def add_events(events: list[CalendarEvent]) -> Response:
    """Persists the given list of events to the database.
    User must have at least manage rights to given booker groups and edit rights to given setter groups.

    :param events: List of events to be persisted
    :return: Persisted events in JSON with updated ids
    """

    verify_logged_in()

    cur_user = get_current_user_id()
    event_bookers: dict[int, list[str]] = {}
    event_setters: dict[int, list[str]] = {}
    event_objs: dict[int, Event] = {}
    for event in events:
        event_bookers = {event.id: []}
        event_setters = {event.id: []}
        booker_groups = event.booker_groups
        setter_groups = event.setter_groups
        if booker_groups is not None:
            for booker_group_str in booker_groups:
                booker_group = UserGroup.get_by_name(booker_group_str)
                if booker_group is not None:
                    verify_group_access(booker_group, manage_access_set)
                event_bookers[event.id].append(booker_group_str)
        if setter_groups is not None:
            for setter_group_str in setter_groups:
                setter_group = UserGroup.get_by_name(setter_group_str)
                if setter_group is not None:
                    verify_group_access(setter_group, edit_access_set)
                event_setters[event.id].append(setter_group_str)

        event_data = Event(
            title=event.title,
            message=event.description,
            location=event.location,
            start_time=event.start,
            end_time=event.end,
            creator_user_id=cur_user,
            max_size=event.max_size,
            signup_before=event.signup_before,
        )
        db.session.add(event_data)
        event_objs[event.id] = event_data

    db.session.flush()

    # To fetch the new ids generated by the database and create appropriate EventGroups
    event_list = []
    for local_id in event_objs:
        event_list.append(
            {
                "id": event_objs[local_id].event_id,
                "title": event_objs[local_id].title,
                "start": event_objs[local_id].start_time,
                "end": event_objs[local_id].end_time,
                "meta": {
                    "signup_before": event_objs[local_id].signup_before,
                    "enrollments": 0,
                    "maxSize": event_objs[local_id].max_size,
                    "location": event_objs[local_id].location,
                    "description": event_objs[local_id].message,
                },
            }
        )
        for booker in event_bookers[local_id]:
            ug = UserGroup.get_by_name(booker)
            if ug is not None:
                db.session.add(
                    EventGroup(
                        event_id=event_objs[local_id].event_id,
                        usergroup_id=ug.id,
                        manager=False,
                    )
                )
        for setter in event_setters[local_id]:
            ug = UserGroup.get_by_name(setter)
            if ug is not None:
                db.session.add(
                    EventGroup(
                        event_id=event_objs[local_id].event_id,
                        usergroup_id=ug.id,
                        manager=True,
                    )
                )

    db.session.commit()

    return json_response(event_list)


@calendar_plugin.put("/events/<int:event_id>")
def edit_event(event_id: int, event: CalendarEvent) -> Response:
    """Edits the event by the given id with the given event

    :param event_id: Event id
    :param event: Updated event
    :return: HTTP 200 if succeeded, otherwise 400
    """
    verify_logged_in()
    if not user_is_event_manager(event_id):
        raise AccessDenied("No permission to edit the event")
    old_event = Event.get_event_by_id(event_id)
    if not old_event:
        raise NotFound()
    old_event.title = event.title
    old_event.location = event.location
    old_event.message = event.description
    old_event.start_time = event.start
    old_event.end_time = event.end
    old_event.max_size = event.max_size
    old_event.signup_before = event.signup_before
    db.session.commit()
    return ok_response()


def user_is_event_manager(event_id: int) -> bool:
    """Checks if current user is a manager of the event

    :param event_id: event id
    :return: True if current user belongs to given event's manager event group,
    or is admin user, or created the event, otherwise false.
    """
    usr = get_current_user_object()
    if usr.is_admin:
        return True
    event = Event.get_event_by_id(event_id)
    if event is None:
        return False
    if event.creator_user_id == get_current_user_id():
        return True

    event_groups = event.event_groups
    for event_group in event_groups:
        ug: UserGroup = event_group.user_group
        if ug is not None:
            if ug in usr.groups and event_group.manager:
                return True
    return False


@calendar_plugin.delete("/events/<int:event_id>")
def delete_event(event_id: int) -> Response:
    """Deletes the event by the given id

    :param event_id: Event id
    :return: HTTP 200 if succeeded, otherwise 404
    """
    verify_logged_in()

    if not user_is_event_manager(event_id):
        raise AccessDenied("No permission to delete the event")

    event = Event.get_event_by_id(event_id)
    user_obj = get_current_user_object()
    if not event:
        raise NotFound()

    enrolled_users = event.enrolled_users
    if len(enrolled_users) > 0:
        send_email_to_enrolled_users(event, user_obj)

    db.session.delete(event)
    db.session.commit()
    return ok_response()


def send_email_to_enrolled_users(event: Event, user_obj: User) -> None:
    """
    Sends email to enrolled users when event is deleted

    :param: event that is about be deleted
    :param: user_obj user who deletes the event
    :return None, or NotExist()
    """
    enrolled_users = event.enrolled_users
    user_accounts = []
    for user_group in enrolled_users:
        user_account = User.query.filter(User.name == user_group.name).one_or_none()
        if user_account is None:
            raise NotExist()
        user_accounts.append(user_account)
    # TODO Should use users own timezone
    start_time = event.start_time.astimezone(fin_timezone).strftime("%d.%m.%Y %H:%M")
    end_time = event.end_time.astimezone(fin_timezone).strftime("%H:%M (UTC %z)")
    event_time = f"{start_time}-{end_time}"
    name = user_obj.name
    msg = f"TIM-Calendar event {event.title} {event_time} has been cancelled by {name}."
    subject = msg
    for user in user_accounts:
        rcpt = user.email
        send_email(rcpt, subject, msg)
    return


@calendar_plugin.put("/bookings")
def update_book_message(event_id: int, booker_msg: str, booker_group: str) -> Response:
    """Updates the booker message in the specific booking

    :param event_id: the id of the event that has the enrollment
    :param booker_msg: Updated message chain to the enrollment
    :param booker_group: the involved booker group of the enrollment
    """
    verify_logged_in()
    event = Event.get_event_by_id(event_id)
    if event is None:
        raise NotFound()

    user_group = UserGroup.get_by_name(booker_group)
    enrollment = Enrollment.get_enrollment_by_ids(event_id, user_group.id)

    if enrollment is not None:
        enrollment.booker_message = booker_msg
        db.session.commit()
    else:
        raise NotFound()

    return ok_response()


@calendar_plugin.post("/bookings")
def book_event(event_id: int, booker_msg: str) -> Response:
    """Books the event for current user's personal user group.
    TODO: implement booking for user's other groups

    :param event_id: Event id
    :param booker_msg: Message left by booker of the event
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

    group_id = -1
    for group in user_obj.groups:
        if group.name == user_obj.name:
            group_id = group.id

    if group_id < 0:
        raise NotExist("User's personal group was not found")  # Should be impossible

    enrollment = Enrollment.get_enrollment_by_ids(event_id, group_id)
    if enrollment is not None:
        raise RouteException("Event is already booked by the same user group")

    enrollment = Enrollment(
        event_id=event_id,
        usergroup_id=group_id,
        enroll_type_id=0,
        booker_message=booker_msg,
    )  # TODO: add enrollment types

    db.session.add(enrollment)
    db.session.commit()
    send_email_to_creator(event_id, True, user_obj)
    return ok_response()


@calendar_plugin.delete("/bookings/<int:event_id>")
def delete_booking(event_id: int) -> Response:
    """Deletes the booking or enrollment to an event for current user's personal user group.

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
        raise NotFound()

    db.session.delete(enrollment)
    db.session.commit()
    send_email_to_creator(event_id, False, user_obj)
    return ok_response()


def send_email_to_creator(event_id: int, msg_type: bool, user_obj: User) -> None:
    """
    Sends an email of cancelled/booked time to creator of the event

    :param: event_id of the event
    :param: msg_type of the message, reservation (True) or cancellation (False)
    :return: None, otherwise NotExist()
    """
    event = Event.get_event_by_id(event_id)
    if not event:
        raise NotExist()
    creator = event.creator
    # TODO Should use users own timezone
    start_time = event.start_time.astimezone(fin_timezone).strftime("%d.%m.%Y %H:%M")
    end_time = event.end_time.astimezone(fin_timezone).strftime("%H:%M (UTC %z)")
    event_time = f"{start_time}-{end_time}"
    name = user_obj.name
    match msg_type:
        case True:
            subject = f"TIM-Calendar reservation {event.title} {event_time} has been booked by {name}."
        case False:
            subject = f"TIM-Calendar reservation {event.title} {event_time} has been cancelled by {name}."
    rcpt = creator.email
    msg = subject
    send_email(rcpt, subject, msg)
    return


register_html_routes(calendar_plugin, class_schema(CalendarHtmlModel), reqs_handle)
