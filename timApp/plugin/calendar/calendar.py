"""
API endpoints for the calendar plugin
"""

__authors__ = [
    "Miika Immonen",
    "Terhi Kamula",
    "Anssi Lepikko",
    "Touko Miettinen",
    "Joose Tikkanen",
]
__license__ = "MIT"
__date__ = "24.5.2022"

import secrets
import uuid
from dataclasses import dataclass, asdict, field
from datetime import date as dtdate
from datetime import datetime
from io import StringIO
from textwrap import wrap
from typing import Literal

from flask import Response, render_template_string, url_for
from sqlalchemy import false, true

from timApp.auth.accesshelper import verify_logged_in, AccessDenied
from timApp.auth.sessioninfo import (
    get_current_user_id,
    get_current_user_object,
)
from timApp.notification.send_email import send_email
from timApp.plugin.calendar.models import Event, EventGroup, Enrollment, EventTag
from timApp.plugin.calendar.models import ExportedCalendar
from timApp.timdb.sqa import db
from timApp.user.groups import verify_group_access
from timApp.user.user import User, edit_access_set
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
    EditorTab,
)

calendar_plugin = TypedBlueprint("calendar_plugin", __name__, url_prefix="/calendar")


@dataclass
class FilterOptions:
    """Calendar markup fields for filtering options"""

    groups: list[str] | None = field(default=None, metadata={"list_type": "delimited"})
    """Group filter. Show events only if the user belongs to one of these event groups."""

    tags: list[str] | None = field(default=None, metadata={"list_type": "delimited"})
    """Tag filter. Show events only the user has one of these tags."""

    fromDate: datetime | None = None
    """Date filter. Show events only starting from this date."""

    toDate: datetime | None = None
    """Date filter. Show events only ending before this date."""

    showBooked: bool = True
    """Whether to always show events that the user is already booked in"""

    includeOwned: bool = False
    """Whether to include events that the user owns"""

    def __post_init__(self) -> None:
        # Special case: if the tags list has only one empty string, treat it as empty list
        # This allows to handle query parameter like &tags= to specify
        # an empty list (as empty string tags are not allowed)
        if self.tags is not None and len(self.tags) == 1 and not self.tags[0]:
            self.tags = []

        # Same for groups
        if self.groups is not None and len(self.groups) == 1 and not self.groups[0]:
            self.groups = []

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class EventTemplate:
    """Calendar markup fields for event template"""

    title: str | None = None
    location: str | None = None
    bookers: list[str] = field(default_factory=list)
    setters: list[str] = field(default_factory=list)
    extraBookers: list[str] = field(default_factory=list)
    capacity: int = 0
    tags: list[str] = field(default_factory=list)

    def to_json(self) -> dict:
        return asdict(self)


@dataclass
class ViewOptions:
    dayStartHour: int = 8
    dayEndHour: int = 20
    segmentDuration: int = 60
    date: dtdate | None = None
    week: int | None = None
    mode: Literal["day", "week", "month"] = "week"


@dataclass
class CalendarMarkup(GenericMarkupModel):
    """Highest level attributes in the calendar markup"""

    filter: FilterOptions = field(default_factory=FilterOptions)
    eventTemplates: dict[str, EventTemplate] = field(default_factory=dict)
    viewOptions: ViewOptions = field(default_factory=ViewOptions)


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
    template_full = """
``` {plugin="calendar"}
#filter:                   # Optional filter options
#    groups:               # Only show events for these groups
#     - group1
#    tags:                 # Only show events with these tags
#     - tag1
#    fromDate: 2022-08-16 20:00:00  # Only show events starting from this date
#    toDate: 2022-08-16 20:00:00    # Only show events ending before this date
#    showBooked: true      # Whether to *always* show events that the user has already booked
#    includeOwned: false   # Whether to include events that the user has created (i.e. "owns")
viewOptions:               # Default view options for the calendar
    dayStartHour: 8        # Time at which the day starts (0-24)
    dayEndHour: 20         # Time at which the day ends (0-24)
    segmentDuration: 60    # Duration of a single time segment (a selectable slot in calendar) in minutes. Allowed values: 15, 20, 30, 60, 120
    week: null             # Week number to show (if not specified, show current week)
    date: null             # Date to show (if not specified, show current date). Has higher priority than week.
    mode: week             # Calendar mode to show (day, week, month)
eventTemplates:            # Event templates for the calendar. Used to create new events.
    Event:                 # Name of the template. Can contain spaces.
        title: Event name  # Name of the event.
        location:          # Location of the event
        bookers:           # List of groups that can see the event in their calendars and book it.
          - bookersgroup
        setters:           # List of groups that can edit the event details.
          - settersgroup
        extraBookers: []   # List of groups that can book the event but who will not affect the capacity.
        capacity: 1        # Maximum number of people that can book the event.
        tags:              # List of tags that can be used to filter events.
          - tag1
```
"""

    template_mini = """
``` {plugin="calendar"}
```
"""

    editor_tabs: list[EditorTab] = [
        {
            "text": "plugins",
            "items": [
                {
                    "text": "Calendar",
                    "items": [
                        {
                            "data": template_mini.strip(),
                            "text": "Calendar (minimal)",
                            "expl": "A minimal calendar that shows all events you can book",
                        },
                        {
                            "data": template_full.strip(),
                            "text": "Calendar (full example)",
                            "expl": "A full example of the calendar plugin",
                        },
                    ],
                },
            ],
        },
    ]
    return {
        "js": ["calendar"],
        "multihtml": True,
        "editor_tabs": editor_tabs,
    }


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

    :return: ICS file that can be exported otherwise 404 if user data does not exist.
    """
    user_data: ExportedCalendar = ExportedCalendar.query.filter_by(
        calendar_hash=key
    ).one_or_none()
    if user_data is None:
        raise NotExist()

    user_obj = user_data.user
    events = events_of_user(user_obj)

    buf = StringIO()
    buf.write("BEGIN:VCALENDAR\r\n")
    buf.write("PRODID:-//TIM-kalenteri//iCal4j 1.0//EN\r\n")
    buf.write("VERSION:2.0\r\n")
    buf.write("CALSCALE:GREGORIAN\r\n")

    def wrap_lines(s: str) -> str:
        return "\r\n ".join(wrap(s, width=60))

    for event in events:
        dts = event.start_time.strftime("%Y%m%dT%H%M%S")
        dtend = event.end_time.strftime("%Y%m%dT%H%M%S")

        buf.write("BEGIN:VEVENT\r\n")
        buf.write(f"DTSTART:{dts}Z\r\n")
        buf.write(f"DTEND:{dtend}Z\r\n")
        buf.write(f"DTSTAMP:{dts}Z\r\n")
        buf.write(f"UID:{uuid.uuid4().hex[:9]}@tim.jyu.fi\r\n")
        buf.write(f"CREATED:{dts}Z\r\n")
        if event.title is None:
            event.title = ""
        buf.write(f"SUMMARY:{wrap_lines(event.title)}\r\n")
        if event.location is None:
            event.location = ""
        buf.write(f"LOCATION:{wrap_lines(event.location)}\r\n")
        if event.message is None:
            event.message = ""
        buf.write(f"DESCRIPTION:{wrap_lines(event.message)}\r\n")
        buf.write("END:VEVENT\r\n")

    buf.write("END:VCALENDAR\r\n")
    result = buf.getvalue()
    return Response(result, mimetype="text/calendar")


def events_of_user(u: User, filter_opts: FilterOptions | None = None) -> list[Event]:
    """
    Fetches users events

    :param: u User to fetch events for
    :return: list of events
    """
    filter_opts = filter_opts or FilterOptions()

    q = Event.query
    event_filter = false()

    # Events come from different places:
    # 1. Events that are created by the user
    if filter_opts.includeOwned:
        event_filter |= Event.creator == u

    # 2. Events that the user is either a booker or setter for
    subquery_event_groups = (
        u.get_groups(include_expired=False)
        .join(EventGroup, EventGroup.usergroup_id == UserGroup.id)
        .with_entities(EventGroup.event_id)
        .subquery()
    )
    event_filter |= Event.event_id.in_(subquery_event_groups)

    # Filter out any tags and groups
    if filter_opts.tags is not None:
        q = q.join(EventTag, Event.tags)
        event_filter &= EventTag.tag.in_(filter_opts.tags)
    if filter_opts.groups is not None:
        q = q.join(EventGroup, Event.event_groups).join(
            UserGroup, EventGroup.usergroup_id == UserGroup.id
        )
        event_filter &= UserGroup.name.in_(filter_opts.groups)

    # Add in all bookend events if asked
    if filter_opts.showBooked:
        enrolled_subquery = (
            u.get_groups(include_expired=False)
            .join(Enrollment, Enrollment.usergroup_id == UserGroup.id)
            .with_entities(Enrollment.event_id)
            .subquery()
        )
        # We have to do this via union so that earlier filters are not applied
        q = q.filter(event_filter)
        q2 = Event.query.filter(Event.event_id.in_(enrolled_subquery))
        q = q.union(q2)
        event_filter = true()

    # Apply date filter to all events
    if filter_opts.fromDate:
        event_filter &= Event.start_time >= filter_opts.fromDate
    if filter_opts.toDate:
        event_filter &= Event.end_time <= filter_opts.toDate

    q = q.filter(event_filter)

    return q.all()


@calendar_plugin.get("/events", model=FilterOptions)
def get_events(opts: FilterOptions) -> Response:
    """Fetches the events created by the user and the events that have a relation to user's groups from the database
    in JSON format

    :return: User's events in JSON format or HTTP 400 if failed
    """
    verify_logged_in()
    cur_user = get_current_user_object()
    events = events_of_user(cur_user, opts)
    # TODO: This needs further optimization
    return json_response(
        [
            e.to_json(with_users=user_is_event_manager(e.event_id), for_user=cur_user)
            for e in events
        ]
    )


@calendar_plugin.get("/events/<int:event_id>/bookers")
def get_event_bookers(event_id: int) -> str | Response:
    """Fetches all enrollments from the database for the given event and returns the full name and email of every
    booker in a html table

    :param event_id: event id
    :return: Full name and email of every booker of the given event in a html table"""

    verify_logged_in()
    if not user_is_event_manager(event_id):
        raise AccessDenied("No permission to see event bookers")

    event = Event.get_by_id(event_id)
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
    extra_booker_groups: list[str] | None = None
    tags: list[str] | None = None
    id: int = -1


@calendar_plugin.post("/events")
def add_events(events: list[CalendarEvent]) -> Response:
    """Saves the calendar events.
    User must have at least manage rights to given booker groups and edit rights to given setter groups.

    :param events: List of events to be persisted
    :return: Saved event information in JSON format
    """
    verify_logged_in()
    cur_user = get_current_user_id()
    event_ug_names = {
        *[ug for event in events if event.booker_groups for ug in event.booker_groups],
        *[ug for event in events if event.setter_groups for ug in event.setter_groups],
        *[
            ug
            for event in events
            if event.extra_booker_groups
            for ug in event.extra_booker_groups
        ],
    }
    event_ugs = UserGroup.query.filter(UserGroup.name.in_(event_ug_names)).all()
    event_ugs_dict = {ug.name: ug for ug in event_ugs}
    event_tags = set(
        [
            tag.strip()
            for event in events
            if event.tags
            for tag in event.tags
            if tag.strip()
        ]
    )
    event_tags_dict = {tag.tag: tag for tag in EventTag.get_or_create(event_tags)}

    result = []
    for event in events:
        bookers = set(event.booker_groups or [])
        setters = set(event.setter_groups or [])
        extra_bookers = set(event.extra_booker_groups or [])
        event_group_names = bookers | setters | extra_bookers
        event_groups = []

        for ug_name in event_group_names:
            if not (ug := event_ugs_dict.get(ug_name)):
                raise NotExist(f"Group '{ug_name}' not found")
            manager = ug_name in setters
            extra = ug_name in extra_bookers
            require_access = edit_access_set if manager else edit_access_set
            verify_group_access(ug, require_access)
            event_groups.append(
                EventGroup(
                    user_group=ug,
                    manager=manager,
                    extra=extra,
                )
            )

        event_data = Event(
            title=event.title,
            message=event.description,
            location=event.location,
            start_time=event.start,
            end_time=event.end,
            creator_user_id=cur_user,
            max_size=event.max_size,
            signup_before=event.signup_before,
            tags=[event_tags_dict[tag] for tag in event.tags]
            if event.tags is not None
            else [],
            event_groups=event_groups,
        )
        result.append(event_data)
        db.session.add(event_data)

    db.session.commit()

    return json_response(result)


@calendar_plugin.put("/events/<int:event_id>")
def edit_event(event_id: int, event: CalendarEvent) -> Response:
    """Edits the event by the given id with the given event

    :param event_id: Event id
    :param event: Updated event
    :return: HTTP 200 if succeeded, otherwise 404
    """
    verify_logged_in()
    if not user_is_event_manager(event_id):
        raise AccessDenied("No permission to edit the event")
    old_event = Event.get_by_id(event_id)
    if not old_event:
        raise NotExist()
    old_event.title = event.title
    old_event.location = event.location
    old_event.message = event.description
    old_event.start_time = event.start
    old_event.end_time = event.end
    old_event.max_size = event.max_size
    old_event.signup_before = event.signup_before
    db.session.commit()
    return ok_response()


# TODO: Refactor
def user_is_event_manager(event_id: int) -> bool:
    """Checks if current user is a manager of the event

    :param event_id: event id
    :return: True if current user belongs to given event's manager event group,
    or is admin user, or created the event, otherwise false.
    """
    usr = get_current_user_object()
    if usr.is_admin:
        return True
    event = Event.get_by_id(event_id)
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

    event = Event.get_by_id(event_id)
    user_obj = get_current_user_object()
    if not event:
        raise NotExist()

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
    :return: HTTP 200 if succeeded, otherwise 404
    """
    verify_logged_in()
    event = Event.get_by_id(event_id)
    if event is None:
        raise NotExist()

    user_group = UserGroup.get_by_name(booker_group)
    enrollment = Enrollment.get_by_event_and_user(event_id, user_group.id)

    if enrollment is not None:
        enrollment.booker_message = booker_msg
        db.session.commit()
    else:
        raise NotExist()

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
    user = get_current_user_object()
    user_group = user.get_personal_group()

    event = Event.get_by_id(event_id)
    if not event:
        raise RouteException(f"Event not found by the id of {event_id}")

    right_info = event.get_enrollment_right(user)
    if not right_info.can_enroll:
        raise AccessDenied("No permission to enroll to the event")

    e_cnt = event.enrollments_count
    if not right_info.extra and e_cnt.normal >= event.max_size:
        raise RouteException("Event is already full")

    enrollment = Enrollment.get_by_event_and_user(event_id, user_group.id)
    if enrollment is not None:
        raise RouteException("Event is already booked by the user")

    # TODO: add enrollment types
    enrollment = Enrollment(
        user_group=user_group,
        enroll_type_id=0,
        booker_message=booker_msg,
        extra=right_info.extra,
    )

    event.enrollments.append(enrollment)
    db.session.commit()
    send_email_to_creator(event_id, True, user)
    return ok_response()


@calendar_plugin.delete("/bookings/<int:event_id>")
def delete_booking(event_id: int) -> Response:
    """Deletes the booking or enrollment to an event for current user's personal user group.

    :param event_id: Event id that matches with the enrollment
    :return: HTTP 200 if succeeded, otherwise 404
    """
    verify_logged_in()
    user = get_current_user_object()
    user_group = user.get_personal_group()

    enrollment = Enrollment.get_by_event_and_user(event_id, user_group.id)
    if not enrollment:
        raise NotExist()

    db.session.delete(enrollment)
    db.session.commit()
    send_email_to_creator(event_id, False, user)
    return ok_response()


def send_email_to_creator(event_id: int, msg_type: bool, user_obj: User) -> None:
    """
    Sends an email of cancelled/booked time to creator of the event

    :param: event_id of the event
    :param: msg_type of the message, reservation (True) or cancellation (False)
    :return: None, otherwise 404
    """
    event = Event.get_by_id(event_id)
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
