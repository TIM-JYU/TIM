from dataclasses import field

from flask import Blueprint
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import dataclass, class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
    GenericAnswerModel,
    register_answer_route,
    PluginAnswerResp,
    PluginAnswerWeb,
)

steps_plugin = Blueprint("steps_plugin", __name__, url_prefix="/steps")


@dataclass
class Step:
    name: str
    description: str | None = None


@dataclass
class StepsPluginMarkup(GenericMarkupModel):
    steps: list[Step] = field(default_factory=list)
    course_phase: int = 0


@dataclass
class StepsPluginStateModel:
    current_step: int
    pass


@dataclass
class StepsPluginInputModel:
    current_step: int
    pass


@dataclass
class StepsPluginHtmlModel(
    GenericHtmlModel[StepsPluginInputModel, StepsPluginMarkup, StepsPluginStateModel]
):
    def get_component_html_name(self) -> str:
        return "tim-steps-plugin"

    def get_static_html(self) -> str:
        return """
            <div>Open by hovering</div>
        """


@dataclass
class StepsPluginAnswerModel(
    GenericAnswerModel[StepsPluginInputModel, StepsPluginMarkup, StepsPluginStateModel]
):
    pass


def reqs_handle() -> PluginReqs:
    return {"js": ["steps"], "multihtml": True}


def answer(args: StepsPluginAnswerModel) -> PluginAnswerResp:
    web: PluginAnswerWeb = {"result": "Saved"}
    result: PluginAnswerResp = {
        "web": web,
        "save": {
            "current_step": args.input.current_step,
        },
    }
    return result


register_html_routes(steps_plugin, class_schema(StepsPluginHtmlModel), reqs_handle)
register_answer_route(steps_plugin, StepsPluginAnswerModel, answer)
