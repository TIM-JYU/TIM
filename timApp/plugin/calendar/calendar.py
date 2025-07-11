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
from enum import Enum
from io import StringIO
from textwrap import wrap
from typing import Literal, Any

from flask import Response, render_template_string, url_for
from marshmallow import missing
from sqlalchemy import false, true, func, select, update

from timApp.auth.accesshelper import (
    verify_logged_in,
    AccessDenied,
    verify_edit_access,
    get_doc_or_abort,
    verify_view_access,
)
from timApp.auth.sessioninfo import (
    get_current_user_id,
    get_current_user_object,
)
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.notification.send_email import send_email
from timApp.plugin.calendar.models import (
    Event,
    EventGroup,
    Enrollment,
    EventTag,
    EnrollmentRight,
)
from timApp.plugin.calendar.models import ExportedCalendar
from timApp.timdb.sqa import db, run_sql
from timApp.user.groups import verify_group_access
from timApp.user.special_group_names import LOGGED_IN_GROUPNAME
from timApp.user.user import User, edit_access_set, manage_access_set
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import json_response, ok_response, text_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.utils import fin_timezone, get_current_time
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
    EditorTab,
)
from tim_common.utils import Missing

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

    showImportant: bool = False
    """Whether to show events marked as important"""

    includeOwned: bool = False
    """Whether to include events that the user owns"""

    includeDocumentEvents: bool = True
    """Whether to include events that are linked to the current document. Requires that the document ID is specified."""

    showBookedByMin: int | None = None
    """Show owned events that are booked by at least the given number of people"""

    docId: int | None = None
    """The document ID to use for the includeDocumentEvents option. If None, no document filtering is done."""

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
    description: str | None = None
    bookers: list[str] = field(default_factory=list)
    setters: list[str] = field(default_factory=list)
    extraBookers: list[str] = field(default_factory=list)
    signupBefore: str | None = None
    sendNotifications: bool = True
    important: bool = False
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
    minWeek: int | None = None
    mode: Literal["day", "week", "month"] = "week"
    advancedUI: bool | None = None


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
    send_notifications: bool = True
    important: bool = False
    tags: list[str] | None = None
    id: int | Missing = missing
    delete: bool | Missing = missing


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
#    showImportant: false  # Whether to *always* show events marked as important
#    includeOwned: false   # Whether to include events that the user has created (i.e. "owns")
#    includeDocumentEvents: true  # Whether to include events that are linked to the current document
viewOptions:               # Default view options for the calendar
    dayStartHour: 8        # Time at which the day starts (0-24)
    dayEndHour: 20         # Time at which the day ends (0-24)
    segmentDuration: 60    # Single segment duration in minutes. Allowed values: 15, 20, 30, 60, 120
    week: null             # Week number to show (if not specified, show current week)
    minWeek: null          # Smallest week number to show. If the current week is smaller than this value, show this week. Has lower priority than week.
    date: null             # Date to show (if not specified, show current date). Has higher priority than week.
    mode: week             # Calendar mode to show (day, week, month)
    # advancedUI: false    # Show large UI (default) or minimal
eventTemplates:            # Event templates for the calendar. Used to create new events.
    Event:                 # Name of the template. Can contain spaces.
        title: Event name  # Name of the event.
        location:          # Location of the event
        description:       # Description of the event
        signupBefore:      # How long before the event start the user can sign up. Either ISO date or ISO duration.
        bookers:           # List of groups that can see the event in their calendars and book it.
          - bookersgroup
        setters:           # List of groups that can edit the event details.
          - settersgroup
        extraBookers: []   # List of groups that can book the event but who will not affect the capacity.
        capacity: 1        # Maximum number of people that can book the event.
        tags:              # List of tags that can be used to filter events.
          - tag1
        sendNotifications: true # Whether to send a notification to the bookers and setters when the event is booked.
        important: false        # Is this event important? Important events are always exported to ICS by default.
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


class CalendarExportFormat(Enum):
    """Export formats for the calendar plugin"""

    ICS = "ics"
    TEMPLATE_JSON = "template-json"


@dataclass
class CalendarExportOptions(FilterOptions):
    format: CalendarExportFormat = field(
        default=CalendarExportFormat.ICS,
        metadata={"by_value": True},
    )


@calendar_plugin.get("/export", model=CalendarExportOptions)
def get_url(opts: CalendarExportOptions) -> Response:
    """Creates a unique URl for user to be used when calendar is exported. User ID is
    bind to specific hash code

    :return: URL with hash code
    """
    verify_logged_in()
    cur_user = get_current_user_object()
    match opts.format:
        case CalendarExportFormat.ICS:
            return export_ical(cur_user)
        case CalendarExportFormat.TEMPLATE_JSON:
            return export_template_json(cur_user, opts)
    raise RouteException(f"Unknown export format: {format}")


def export_template_json(user: User, opts: CalendarExportOptions) -> Response:
    """
    Exports the calendar as importable (i.e. template) JSON

    :param user: User to export the calendar for
    :param opts: Options for the export
    :return:
    """
    events = events_of_user(user, opts)

    result = []
    for event in events:
        cal_event = CalendarEvent(
            title=event.title,
            start=event.start_time,
            end=event.end_time,
            signup_before=event.signup_before,
            max_size=event.max_size,
            location=event.location,
            description=event.message,
            tags=[tag.tag for tag in event.tags],
            send_notifications=event.send_notifications,
            important=event.important,
            id=event.event_id,
        )

        bookers = []
        setters = []
        extra_bookers = []

        for eg in event.event_groups:
            if eg.manager:
                setters.append(eg.user_group.name)
            if not eg.manager:
                bookers.append(eg.user_group.name)
            if eg.extra:
                extra_bookers.append(eg.user_group.name)

        cal_event.booker_groups = bookers
        cal_event.setter_groups = setters
        cal_event.extra_booker_groups = extra_bookers

        result.append(cal_event)

    return json_response(result)


def export_ical(user: User) -> Response:
    """
    Generate an ICS link for the user's calendar

    :param user: User to generate ICS link for
    :return:
    """
    user_data: ExportedCalendar | None = (
        run_sql(select(ExportedCalendar).filter(ExportedCalendar.user_id == user.id))
        .scalars()
        .one_or_none()
    )
    if user_data is not None:
        hash_code = user_data.calendar_hash
        url = url_for("calendar_plugin.get_ical", key=hash_code, _external=True)
        return text_response(url)
    hash_code = secrets.token_urlsafe(16)
    user_data = ExportedCalendar(
        user_id=user.id,
        calendar_hash=hash_code,
    )
    db.session.add(user_data)
    db.session.commit()
    url = url_for("calendar_plugin.get_ical", key=hash_code, _external=True)
    return text_response(url)


@calendar_plugin.post("/import")
def import_events(
    events: list[CalendarEvent], origin_doc_id: int | None = None
) -> Response:
    verify_logged_in()
    doc = get_doc_or_abort(origin_doc_id) if origin_doc_id else None
    if doc and not doc.is_original_translation:
        doc = doc.src_doc
    save_events(get_current_user_object(), events, True, save_origin=doc)
    db.session.commit()
    return ok_response()


@dataclass
class ICalFilterOptions:
    """Options for filtering the ICS export"""

    key: str
    showImportant: bool = True
    includeOwned: bool = False
    showBookedByMin: int | None = None


@calendar_plugin.get("/ical", model=ICalFilterOptions)
def get_ical(opts: ICalFilterOptions) -> Response:
    """Fetches users events in a ICS format. User ID is sorted out from hash code from query parameter

    :return: ICS file that can be exported otherwise 404 if user data does not exist.
    """
    user_data: ExportedCalendar | None = (
        run_sql(select(ExportedCalendar).filter_by(calendar_hash=opts.key))
        .scalars()
        .one_or_none()
    )
    if user_data is None:
        raise NotExist()

    user_obj = user_data.user
    # TODO: Add proper per-ICS URL filtering (e.g. save FilterOptions to the database)
    events = events_of_user(
        user_obj,
        FilterOptions(
            tags=[],
            groups=[],
            showImportant=opts.showImportant,
            showBookedByMin=opts.showBookedByMin,
            includeOwned=opts.includeOwned,
        ),
    )

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

    stmt = select(Event.event_id)
    event_queries = []
    event_filter: Any = false()

    # Events come from different places:
    # 1. Events that are created by the user
    if filter_opts.includeOwned:
        event_filter |= Event.creator == u

    if filter_opts.includeDocumentEvents and filter_opts.docId:
        doc = DocEntry.find_by_id(filter_opts.docId)
        if doc:
            # Resolve translation, because we want to add events for the original document
            if not doc.is_original_translation:
                doc = doc.src_doc
            if verify_view_access(doc, require=False):
                event_filter |= Event.origin_doc_id == doc.id

    # 2. Events that the user is either a booker or setter for
    # TODO: Add a flag to also include special groups (i.e. to show global events)
    subquery_event_groups_all = (
        u.get_groups(include_expired=False, include_special=False)
        .join(EventGroup, EventGroup.usergroup_id == UserGroup.id)
        .with_only_columns(EventGroup.event_id)
    )
    subquery_event_groups = subquery_event_groups_all
    # Apply group filter if there is one
    if filter_opts.groups is not None:
        # noinspection PyUnresolvedReferences
        subquery_event_groups = subquery_event_groups.filter(
            UserGroup.name.in_(filter_opts.groups)
        )
    # noinspection PyUnresolvedReferences
    event_filter |= Event.event_id.in_(subquery_event_groups)

    # Filter out any tags and groups
    if filter_opts.tags is not None:
        stmt = stmt.join(EventTag, Event.tags)
        # noinspection PyUnresolvedReferences
        event_filter &= EventTag.tag.in_(filter_opts.tags)

    stmt = stmt.filter(event_filter)

    if filter_opts.showImportant:
        # noinspection PyUnresolvedReferences
        important_q = select(Event.event_id).filter(
            Event.event_id.in_(subquery_event_groups_all) & Event.important.is_(True)
        )
        event_queries.append(important_q)

    # Add in all bookend events if asked
    if filter_opts.showBooked:
        enrolled_subquery = (
            u.get_groups(include_expired=False)
            .join(Enrollment, Enrollment.usergroup_id == UserGroup.id)
            .with_only_columns(Enrollment.event_id)
        )
        # noinspection PyUnresolvedReferences
        booked_query = select(Event.event_id).filter(
            Event.event_id.in_(enrolled_subquery)
        )
        event_queries.append(booked_query)

    if filter_opts.showBookedByMin is not None:
        booked_min_subquery = (
            select(Event)
            .filter(Event.creator_user_id == u.id)
            .outerjoin(Enrollment)
            .group_by(Event.event_id)
            .with_only_columns(
                Event.event_id, func.count(Enrollment.event_id).label("count")
            )
        ).subquery()
        booked_min_query = (
            select(Event.event_id)
            .select_from(booked_min_subquery)
            .join(Event, Event.event_id == booked_min_subquery.c.event_id)
            .filter(booked_min_subquery.c.count >= filter_opts.showBookedByMin)
        )
        event_queries.append(booked_min_query)

    timing_filter: Any = true()
    # Apply date filter to all events
    if filter_opts.fromDate:
        timing_filter &= Event.start_time >= filter_opts.fromDate
    if filter_opts.toDate:
        timing_filter &= Event.end_time <= filter_opts.toDate

    if event_queries:
        tmp = stmt.union(*event_queries)
        main_stmt = select(Event).filter(Event.event_id.in_(tmp))
    else:
        main_stmt = stmt.with_only_columns(Event)
    main_stmt = main_stmt.filter(timing_filter)

    return run_sql(main_stmt).scalars().all()  # type: ignore


@calendar_plugin.get("/events", model=FilterOptions)
def get_events(opts: FilterOptions) -> Response:
    """Fetches the events created by the user and the events that have a relation to user's groups from the database
    in JSON format

    :return: User's events in JSON format or HTTP 400 if failed
    """
    cur_user = get_current_user_object()
    events = events_of_user(cur_user, opts)
    # TODO: This needs further optimization
    return json_response(
        [
            e.to_json(
                with_users=user_is_event_manager(e.event_id),
                for_user=cur_user,
            )
            for e in events
        ]
    )


@calendar_plugin.get("/events/<int:event_id>")
def get_event(event_id: int) -> Response:
    """Fetches the event with the given id from the database in JSON format

    :param event_id: event id
    :return: Event in JSON format
    """
    event = Event.get_by_id(event_id)
    if event is None:
        raise NotExist(f"Event not found by the id of {event_id}")

    cur_user = get_current_user_object()
    right = event.get_enrollment_right(cur_user)
    if not right.can_see:
        raise AccessDenied("No permission to see event details")

    return json_response(
        event.to_json(
            with_users=right.can_manage_event,
            for_user=cur_user,
            desc_as_md=True,
        )
    )


@calendar_plugin.get("/events/<int:event_id>/bookers")
def get_event_bookers(event_id: int, json: bool = False) -> str | Response:
    """Fetches all enrollments from the database for the given event and returns the full name and email of every
    booker in a html table

    :param event_id: event id
    :param json: If true, return in JSON format instead of HTML
    :return: Full name and email of every booker of the given event in a html table"""

    verify_logged_in()
    if not user_is_event_manager(event_id):
        raise AccessDenied("No permission to see event bookers")

    event = Event.get_by_id(event_id)
    if event is None:
        no_event_found = f"Event not found by the id of {event_id}"
        raise NotExist(no_event_found)

    bookers_info = []
    enrollments = event.enrollments
    for enrollment in enrollments:
        bookers = enrollment.usergroup.users
        for booker in bookers:
            bookers_info.append(
                {
                    "name": booker.name,
                    "full_name": booker.real_name,
                    "email": booker.email,
                    "isExtra": "x" if enrollment.extra else "",
                }
            )

    bookers_info = sorted(bookers_info, key=lambda x: 0 if x["isExtra"] else 1)

    if json:
        return json_response(bookers_info)

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
                <th>Extra?</th>
            </tr>
            {% for booker in bookers_info %}
                <tr>
                    <td>{{ booker.full_name }}</td>
                    <td>{{ booker.email }}</td>
                    <td>{{ booker.isExtra }}</td>
                </tr>
            {% endfor %}
        </table>
    </body>
    """,
        bookers_info=bookers_info,
    )


def _replace_group_wildcards(groups: set[str]) -> set[str]:
    if "*" in groups:
        groups.remove("*")
        groups.add(LOGGED_IN_GROUPNAME)
    return groups


def save_events(
    user: User,
    events: list[CalendarEvent],
    modify_existing: bool = False,
    save_origin: DocInfo | None = None,
) -> list[Event]:
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

    _replace_group_wildcards(event_ug_names)

    # noinspection PyUnresolvedReferences
    event_ugs = (
        run_sql(select(UserGroup).filter(UserGroup.name.in_(event_ug_names)))
        .scalars()
        .all()
    )
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

    def get_event_groups(cal_event: CalendarEvent) -> list[EventGroup]:
        bookers = _replace_group_wildcards(set(cal_event.booker_groups or []))
        setters = _replace_group_wildcards(set(cal_event.setter_groups or []))
        extra_bookers = _replace_group_wildcards(
            set(cal_event.extra_booker_groups or [])
        )
        event_group_names = bookers | setters | extra_bookers

        event_groups = []

        for ug_name in event_group_names:
            if not (ug := event_ugs_dict.get(ug_name)):
                raise NotExist(f"Group '{ug_name}' not found")

            manager = ug_name in setters
            extra = ug_name in extra_bookers

            if ug_name == LOGGED_IN_GROUPNAME:
                if not save_origin:
                    raise AccessDenied(
                        "Creating global events is not permitted for now."
                    )
                if not verify_edit_access(save_origin, require=False):
                    raise AccessDenied(
                        "You must have edit access to the document to create document events."
                    )
                if manager:
                    raise RouteException(
                        "Global setters are not allowed at the moment."
                    )
                if extra:
                    raise RouteException("Global extras are not allowed at the moment.")
            else:
                require_access = edit_access_set if manager else manage_access_set
                verify_group_access(ug, require_access)

            event_groups.append(
                EventGroup(
                    user_group=ug,
                    manager=manager,
                    extra=extra,
                )
            )
        return event_groups

    def add_event(cal_event: CalendarEvent) -> Event:
        event_groups = get_event_groups(cal_event)
        event_data = Event(
            title=cal_event.title,
            message=cal_event.description,
            location=cal_event.location,
            start_time=cal_event.start,
            end_time=cal_event.end,
            creator_user_id=user.id,
            max_size=cal_event.max_size,
            signup_before=cal_event.signup_before,
            send_notifications=cal_event.send_notifications,
            important=cal_event.important,
            tags=[event_tags_dict[tag] for tag in cal_event.tags]
            if cal_event.tags is not None
            else [],
            event_groups=event_groups,
            origin_doc_id=save_origin.id if save_origin else None,
        )
        db.session.add(event_data)
        return event_data

    # TODO: This could be probably deduplicated further
    # noinspection DuplicatedCode
    def update_event(cal_event: CalendarEvent, event: Event) -> Event:
        event.title = cal_event.title
        event.start_time = cal_event.start
        event.end_time = cal_event.end
        event.signup_before = cal_event.signup_before
        event.max_size = cal_event.max_size
        event.location = cal_event.location
        event.message = cal_event.description
        event.send_notifications = cal_event.send_notifications
        event.important = cal_event.important
        event.tags = (
            [event_tags_dict[tag] for tag in cal_event.tags] if cal_event.tags else []
        )
        event.event_groups = get_event_groups(cal_event)

        return event

    result = []
    for calendar_event in events:
        if calendar_event.id is not missing:
            if not modify_existing:
                raise AccessDenied("Cannot modify existing events via this route")
            event: Event | None = (
                run_sql(select(Event).filter_by(event_id=calendar_event.id).limit(1))
                .scalars()
                .first()
            )
            if not event:
                raise NotExist(f"Event with id {calendar_event.id} not found")
            rights = event.get_enrollment_right(user)
            if not rights.can_manage_event:
                raise AccessDenied(
                    f"No permission to modify event with id {calendar_event.id}"
                )
            if calendar_event.delete:
                db.session.delete(event)
                continue
            result.append(update_event(calendar_event, event))
        else:
            result.append(add_event(calendar_event))

    return result


@calendar_plugin.post("/events")
def add_events(
    events: list[CalendarEvent],
    origin_doc_id: int | None = None,
) -> Response:
    """Saves the calendar events.
    User must have at least manage rights to given booker groups and edit rights to given setter groups.

    :param events: List of events to be persisted
    :param origin_doc_id: Document ID from which the events are created
    :return: Saved event information in JSON format
    """
    verify_logged_in()
    doc = get_doc_or_abort(origin_doc_id) if origin_doc_id else None
    if doc and not doc.is_original_translation:
        doc = doc.src_doc
    result = save_events(get_current_user_object(), events, save_origin=doc)
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
    # noinspection DuplicatedCode
    old_event.title = event.title
    old_event.location = event.location
    old_event.message = event.description
    old_event.start_time = event.start
    old_event.end_time = event.end
    old_event.max_size = event.max_size
    old_event.signup_before = event.signup_before
    old_event.send_notifications = event.send_notifications
    old_event.important = event.important
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
    if enrolled_users:
        send_email_to_enrolled_users(
            event,
            "{event_title} {event_time} has been cancelled.",
            f"TIM-Calendar event {{event_title}} {{event_time}} has been cancelled by {user_obj.real_name} ({user_obj.name}).",
        )
        enrolled_users_contacts = []
        for enrolled_user in enrolled_users:
            if enrolled_user.is_personal_group:
                enrolled_users_contacts.append(
                    f"{enrolled_user.personal_user.real_name} ({enrolled_user.personal_user.name}), {enrolled_user.personal_user.email}"
                )
            else:
                enrolled_users_contacts.append(f"Group {enrolled_user.name}")
        enrolled_users_list = "\n ".join(enrolled_users_contacts)
        send_email_to_creator(
            event_id,
            CalendarEmailEvent.Deleted,
            user_obj,
            f"The following users and groups were enrolled to the event:\n\n{enrolled_users_list}",
        )

    db.session.delete(event)
    db.session.commit()
    return ok_response()


def send_email_to_enrolled_users(
    event: Event, subject: str, message: str, include_creator: bool = False
) -> None:
    """
    Sends email to enrolled users

    :param: event that is about be deleted
    :param: user_obj user who deletes the event
    :return None, or NotExist()
    """
    if not event.send_notifications:
        return
    enrolled_users = event.enrolled_users
    user_accounts = []
    for user_group in enrolled_users:
        user_account = (
            run_sql(select(User).filter(User.name == user_group.name))
            .scalars()
            .one_or_none()
        )
        if user_account is None:
            raise NotExist()
        user_accounts.append(user_account)
    if include_creator:
        user_accounts.append(event.creator)
    # TODO Should use users own timezone
    start_time = event.start_time.astimezone(fin_timezone).strftime("%d.%m.%Y %H:%M")
    end_time = event.end_time.astimezone(fin_timezone).strftime("%H:%M (UTC %z)")
    event_time = f"{start_time}-{end_time}"
    subject = subject.format(subject, event_title=event.title, event_time=event_time)
    message = message.format(message, event_title=event.title, event_time=event_time)
    # TODO: This is very inefficient, send instead one mail using BCC
    for user in user_accounts:
        send_email(user.email, subject, message)


# TODO: No need to pass booker group, deduce it from the caller
@calendar_plugin.put("/bookings")
def update_book_message(event_id: int, booker_msg: str, booker_group: str) -> Response:
    """Updates the booker message in the specific booking

    :param event_id: the id of the event that has the enrollment
    :param booker_msg: New message to add to the enrollment
    :param booker_group: the involved booker group of the enrollment
    :return: HTTP 200 if succeeded, otherwise 404
    """
    verify_logged_in()
    event = Event.get_by_id(event_id)
    if event is None:
        raise NotExist()

    user = get_current_user_object()
    # TODO: Don't save as fixed timezone, save as UTC instead
    now = get_current_time().astimezone(fin_timezone)
    new_message = f"{user.name} {now.strftime('%d.%m.%Y %H:%M')}: {booker_msg}"

    right: EnrollmentRight = event.get_enrollment_right(user)
    if not right.can_use:
        raise AccessDenied("You are not allowed to post messages to this event")

    user_group = UserGroup.get_by_name(booker_group)
    enrollment = Enrollment.get_by_event_and_user(event_id, user_group.id)
    if not enrollment:
        raise NotExist()

    run_sql(
        update(Enrollment)
        .where(Enrollment.event_id == enrollment.event_id)
        .values(
            {"booker_message": Enrollment.booker_message + f"\n{new_message}"},
        )
        .execution_options(synchronize_session="fetch")
    )
    db.session.commit()

    enrolled_users = event.enrolled_users
    if enrolled_users:
        send_email_to_enrolled_users(
            event,
            f"{user.real_name} posted a message to event {{event_title}} {{event_time}}",
            f"{user.real_name} ({user.name}) posted a message to TIM calendar event "
            f"{{event_title}} {{event_time}}:\n\n{booker_msg}",
            include_creator=True,
        )

    return json_response({"bookMessage": enrollment.booker_message})


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
        usergroup=user_group,
        enroll_type_id=0,
        booker_message=booker_msg,
        extra=right_info.extra,
    )

    event.enrollments.append(enrollment)
    db.session.commit()
    send_email_to_creator(event_id, CalendarEmailEvent.Booked, user)
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
    send_email_to_creator(event_id, CalendarEmailEvent.Cancelled, user)
    return ok_response()


class CalendarEmailEvent(Enum):
    Booked = "booked"
    Cancelled = "cancelled"
    Deleted = "deleted"


def send_email_to_creator(
    event_id: int,
    event_type: CalendarEmailEvent,
    user_obj: User,
    extra_msg: str | None = None,
) -> None:
    """
    Sends an email of cancelled/booked time to creator of the event

    :param: event_id ID the event
    :param: event_type Event type to inform of
    """
    event = Event.get_by_id(event_id)
    if not event:
        raise NotExist()
    if not event.send_notifications:
        return
    creator = event.creator
    # TODO Should use users own timezone
    start_time = event.start_time.astimezone(fin_timezone).strftime("%d.%m.%Y %H:%M")
    end_time = event.end_time.astimezone(fin_timezone).strftime("%H:%M (UTC %z)")
    event_time = f"{start_time}-{end_time}"
    name = user_obj.name
    rcpt = creator.email
    msg = f"TIM-Calendar reservation {event.title} {event_time} has been {event_type.value} by {name}."
    # TODO: Subject should be shorter
    subject = msg
    if extra_msg:
        msg += f"\n\n{extra_msg}"
    send_email(rcpt, subject, msg)


register_html_routes(calendar_plugin, class_schema(CalendarHtmlModel), reqs_handle)
