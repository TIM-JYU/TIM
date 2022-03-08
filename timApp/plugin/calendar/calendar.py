from dataclasses import dataclass, asdict

from flask import Response

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


register_html_routes(calendar_plugin, class_schema(CalendarHtmlModel), reqs_handle)
