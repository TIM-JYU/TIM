import json
from dataclasses import dataclass, asdict

from flask import Response, Request

from timApp.auth.accesshelper import verify_logged_in
from timApp.auth.sessioninfo import get_current_user_id
from timApp.plugin.calendar.models import Event
from timApp.timdb.sqa import db
from timApp.util.flask.responsehelper import json_response
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
class CalendarEvents:
    title: str

    def to_json(self) -> dict:
        return asdict(self)


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


@calendar_plugin.post("/add")
def add_events(events: str) -> Response:
    verify_logged_in()
    cur_user = get_current_user_id()
    events_json = json.loads(events)
    print(events_json[0]["start"])

    for event in events_json:
        event = Event(
            title=event["title"],
            start_time=event["start"],
            end_time=event["end"],
            creator_user_id=cur_user,
        )
        db.session.add(event)

    db.session.commit()
    return json_response(events)


register_html_routes(calendar_plugin, class_schema(CalendarHtmlModel), reqs_handle)
