import secrets
import uuid
from dataclasses import dataclass, asdict, field
from datetime import datetime
from io import StringIO

from flask import Response, render_template_string, make_response
from werkzeug.exceptions import NotFound

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import (
    get_current_user_id,
    get_current_user_object,
)
from timApp.notification.send_email import send_email
from timApp.plugin.calendar.models import Event, EventGroup, Enrollment, EnrollmentType
from timApp.plugin.calendar.models import ExportedCalendar
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_access
from timApp.user.user import User, manage_access_set, view_access_set
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException, NotExist
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

    types = EnrollmentType.query.filter(
        EnrollmentType.enroll_type_id == 0
    ).all()  # Remember to add filters here if you add new enrollment types
    if len(types) == 0:
        db.session.add(
            EnrollmentType(enroll_type_id=0, enroll_type="booking")
        )  # TODO: proper enrollment types
        db.session.commit()


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
    domain = app.config["TIM_HOST"]
    url = domain + "/calendar/ical?key="
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
def get_ical(key: str) -> Response:
    """Fetches users events in a ICS format. User ID is sorted out from hash code from query parameter

    :return: ICS file that can be exported
    """
    user_data: ExportedCalendar = ExportedCalendar.query.filter(
        ExportedCalendar.calendar_hash == key
    ).one_or_none()
    if user_data is None:
        raise NotFound()
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
    """
    Splits long strings to n char lines

    :return: str where lines are separated with \r\n + whitespace
    """
    n = 60
    if len(str_to_split) <= n:
        return str_to_split
    lines = [str_to_split[i : i + n] for i in range(0, len(str_to_split), n)]
    new_str = ""
    index = 0
    for line in lines:
        index += 1
        if index == len(lines):
            new_str = new_str + line
            break
        new_str = new_str + line + "\r\n "
    return new_str


@calendar_plugin.get("/events")
def get_events() -> Response:
    """Fetches the events created by the user and the events that have a relation to user's groups from the database
    in JSON format

    :return: User's events in JSON format or HTTP 400 if failed
    """
    verify_logged_in()
    cur_user = get_current_user_id()

    events: list[Event] = Event.query.filter(Event.creator_user_id == cur_user).all()

    user_obj = get_current_user_object()

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
                print("Event not found by the id of", group_event.event_id)

    event_objs = []
    for event in events:
        event_optional = Event.get_event_by_id(event.event_id)
        if event_optional is not None:
            event_obj = event_optional
            enrollments = len(event_obj.enrolled_users)
            booker_groups = event_obj.enrolled_users
            groups = []
            # event_groups = event_obj.groups_in_event
            event_groups: list[EventGroup] = EventGroup.query.filter(
                event_obj.event_id == EventGroup.event_id
            ).all()
            for event_group in event_groups:
                user_group: UserGroup = UserGroup.query.filter(
                    UserGroup.id == event_group.usergroup_id
                ).one_or_none()
                if user_group is None:
                    continue  # should be impossible

                for group in booker_groups:
                    users = []

                    for user in group.users:

                        if (
                            user.id == cur_user or user_obj.is_admin
                        ):  # Fetch user's own bookings, pass authorization if admin
                            users.append(
                                {
                                    "id": user.id,
                                    "name": user.real_name,
                                    "email": user.email,
                                }
                            )
                        elif (
                            user_group in user_obj.groups and event_group.manager
                        ):  # Only fetch event bookers when current user belongs to manager event group
                            users.append(
                                {
                                    "id": user.id,
                                    "name": user.real_name,
                                    "email": user.email,
                                }
                            )
                    groups.append({"name": group.name, "users": users})
                break  # Add booker info only once if user belongs to multiple manager groups
            event_objs.append(
                {
                    "id": event_obj.event_id,
                    "title": event_obj.title,
                    "start": event_obj.start_time,
                    "end": event_obj.end_time,
                    "meta": {
                        "description": event_obj.message,
                        "enrollments": enrollments,
                        "maxSize": event_obj.max_size,
                        "location": event_obj.location,
                        "booker_groups": groups,
                        "signup_before": event_obj.signup_before,
                    },
                }
            )
        else:
            print(
                "Error fetching the event by the id of", event.event_id
            )  # should be never possible

    return json_response(event_objs)


@calendar_plugin.get("/events/<int:event_id>/bookers")
def get_event_bookers(event_id: int) -> str | Response:
    """Fetches all enrollments from the database for the given event and returns the full name and email of every
    booker in a html table

    :param event_id: event id
    :return: Full name and email of every booker of the given event in a html table"""

    verify_logged_in()
    usr = get_current_user_object()
    event = Event.get_event_by_id(event_id)
    if event is None:
        raise NotExist(f"Event not found by the id of {0}".format(event_id))

    event_groups: list[EventGroup] = EventGroup.query.filter(
        event_id == EventGroup.event_id
    ).all()
    usr_manager_groups = 0
    for event_group in event_groups:
        user_group: UserGroup = UserGroup.query.filter(
            UserGroup.id == event_group.usergroup_id
        ).one_or_none()
        if user_group is None:
            continue  # should be impossible
        if (
            user_group in usr.groups and event_group.manager
        ):  # User needs to belong to at least one manager event group
            usr_manager_groups += 1
    if usr_manager_groups == 0 and not usr.is_admin:
        return make_response(
            {"error": f"No permission to see event bookers."},
            403,
        )
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
    description: str
    location: str
    start: datetime
    end: datetime
    signup_before: datetime
    max_size: int = 1
    booker_groups: list[str] | None = None
    setter_groups: list[str] | None = None
    event_groups: list[str] | None = None
    id: int = -1


@calendar_plugin.post("/events")
def add_events(events: list[CalendarEvent]) -> Response:
    """Persists the given list of events to the database.
    User must have at least manage rights to given booker-groups set in the events.

    :param events: List of events to be persisted
    :return: Persisted events in JSON with updated ids
    """

    verify_logged_in()

    setters = []
    bookers = []
    usr = get_current_user_object()
    user_is_manager = usr.is_admin
    for event in events:
        booker_groups = event.booker_groups
        setter_groups = event.setter_groups
        if booker_groups is not None:
            for booker_group_str in booker_groups:
                booker_group = UserGroup.get_by_name(booker_group_str)
                if booker_group is not None:
                    verify_group_access(booker_group, manage_access_set)
                bookers.append({"booker": booker_group_str, "event_id": event.id})
        if len(setter_groups) > 0:
            for setter_group_str in setter_groups:
                setter_group = UserGroup.get_by_name(setter_group_str)
                if setter_group in usr.groups:
                    user_is_manager = True
                setters.append({"setter": setter_group_str, "event_id": event.id})
            if not user_is_manager:
                return make_response(
                    {"error": f"No access for any of the setter groups."},
                    403,
                )

    cur_user = get_current_user_id()
    added_events = []
    for event in events:
        # groups = []
        group_names = event.event_groups
        if group_names is not None:
            group_name_strs: list[str] = group_names
            for event_group in group_name_strs:
                group = UserGroup.get_by_name(event_group)
                if group is not None:
                    # groups.append(group)
                    print("f")

        event_data = Event(
            title=event.title,
            message=event.description,
            location=event.location,
            start_time=event.start,
            end_time=event.end,
            creator_user_id=cur_user,
            # groups_in_event=groups,
            max_size=event.max_size,
            signup_before=event.signup_before,
        )
        db.session.add(event_data)
        added_events.append(
            {
                "event": event_data,
                "local_id": event.id,
            }
        )

    db.session.commit()

    # To fetch the new ids generated by the database
    event_list = []
    for event in added_events:
        event_list.append(
            {
                "id": event["event"].event_id,
                "title": event["event"].title,
                "start": event["event"].start_time,
                "end": event["event"].end_time,
                "meta": {
                    "signup_before": event["event"].signup_before,
                    "enrollments": 0,
                    "maxSize": event["event"].max_size,
                    "location": event["event"].location,
                    "message": event["event"].message,
                },
            }
        )
        for setter in setters:
            if setter["event_id"] == event["local_id"]:
                ug = UserGroup.get_by_name(setter["setter"])
                db.session.add(
                    EventGroup(
                        event_id=event["event"].event_id,
                        usergroup_id=ug.id,
                        manager=True,
                    )
                )
        for booker in bookers:
            if booker["event_id"] == event["local_id"]:
                ug = UserGroup.get_by_name(booker["booker"])
                db.session.add(
                    EventGroup(
                        event_id=event["event"].event_id,
                        usergroup_id=ug.id,
                        manager=False,
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
    old_event = Event.get_event_by_id(event_id)
    if not old_event:
        raise RouteException("Event not found")
    old_event.title = event.title
    old_event.location = event.location
    old_event.message = event.description
    old_event.start_time = event.start
    old_event.end_time = event.end
    db.session.commit()
    return ok_response()


@calendar_plugin.delete("/events/<int:event_id>")
def delete_event(event_id: int) -> Response:
    """Deletes the event by the given id

    :param event_id: Event id
    :return: HTTP 200 if succeeded, otherwise 404
    """
    verify_logged_in()
    event = Event.get_event_by_id(event_id)
    if not event:
        raise NotFound()
    usr = get_current_user_object()

    event_groups: list[EventGroup] = EventGroup.query.filter(
        EventGroup.event_id == event_id
    ).all()
    user_is_manager = usr.is_admin
    for event_group in event_groups:
        ug: UserGroup = UserGroup.query.filter(
            event_group.usergroup_id == UserGroup.id
        ).one_or_none()
        if ug is not None:
            if ug in usr.groups and event_group.manager:
                user_is_manager = True

    if not user_is_manager:
        return make_response({"error": "No permission to delete the event"}, 403)
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
        event_id=event_id, usergroup_id=group_id, enroll_type_id=0
    )  # TODO: add enrollment types

    db.session.add(enrollment)
    db.session.commit()
    send_email_to_creator(event_id, True, user_obj)
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
    send_email_to_creator(event_id, False, user_obj)
    return ok_response()


def send_email_to_creator(event_id: int, msg_type: bool, user_obj: User) -> Response:
    """
    Sends an email of cancelled/booked time to creator of the event

    :param: event_id of the event
    :param: msg_type of the message, reservation (True) or cancellation (False)
    :return: HTTP 200 if succeeded, otherwise 400
    """
    event = Event.get_event_by_id(event_id)
    if not event:
        raise NotExist()
    creator = event.creator
    start_time = event.start_time.strftime("%d.%m.%Y %H:%M")
    end_time = event.end_time.strftime("%H:%M")
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
    return ok_response()


register_html_routes(calendar_plugin, class_schema(CalendarHtmlModel), reqs_handle)
