from dataclasses import field

from flask import Blueprint, Response, request

from timApp.auth.sessioninfo import logged_in, get_current_user, get_current_user_group
from timApp.util.flask.requesthelper import RouteException
from timApp.util.flask.responsehelper import json_response
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
from timApp.steps.routes import StepsPhase
from tim_common.utils import Missing

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


@steps_plugin.post("/switch")
def switch_steps() -> Response:
    if not logged_in():
        raise RouteException("You have to be logged in to switch steps state.")

    # TODO: tsekkaa, onko opettaja ylipäätään luonut ko. steppijuttua
    # TODO: tarkista
    request_data = request.get_json()
    user_group = get_current_user_group()

    step = StepsPhase.find_by_task(
        doc_id=request_data["docId"],
        name=request_data["name"],
        user_group=user_group,
    )

    if not step:
        step = StepsPhase.create(
            doc_id=request_data["docId"],
            name=request_data["name"],
            current_phase=request_data["currentStep"],
        )

    step.change_current_step(request_data["currentStep"])

    print(step)
    print(request_data)
    print(user_group)
    response = Response()
    return response


# ryhmä --> ryhmän_dashboard --> stepkomponenttti
# TAI ryhmäkokoelma --> ryhmäkokoelma sisältää opettajien ryhmät + työryhmät --> filtteröidään tieto, kun user tulee tähän reittiin
# Esim: oscar25 --> oscar_opet + oscar_apinat + oscar_kirahvit ... ->


@steps_plugin.get("<int:doc_id>/<string:name>")
def get_step_info(doc_id: int, name: str) -> Response:
    if not logged_in():
        raise RouteException("You have to be logged in to switch steps state.")

    user_group = get_current_user_group()
    step = StepsPhase.find_by_task(
        doc_id=doc_id,
        name=name,
        user_group=user_group,
    )

    print(step)
    response = json_response(step)
    return response


register_html_routes(steps_plugin, class_schema(StepsPluginHtmlModel), reqs_handle)
# register_answer_route(steps_plugin, StepsPluginAnswerModel, answer)
