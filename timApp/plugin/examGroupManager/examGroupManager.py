from dataclasses import dataclass, field

from marshmallow import missing

from timApp.util.flask.typedblueprint import TypedBlueprint
from tim_common.markupmodels import GenericMarkupModel
from tim_common.marshmallow_dataclass import class_schema
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginReqs,
    register_html_routes,
)
from tim_common.utils import Missing

exam_group_manager_plugin = TypedBlueprint(
    "exam_group_manager_plugin", __name__, url_prefix="/examGroupManager"
)


@dataclass
class ExamGroupManagerMarkup(GenericMarkupModel):
    groupsPath: str | Missing = missing
    extraInfoTitle: str | Missing = missing
    showAllGroups: bool = False
    managers: list[str] = field(default_factory=list)


@dataclass
class ExamGroupManagerStateModel:
    pass


@dataclass
class ExamGroupManagerInputModel:
    pass


@dataclass
class ExamGroupManagerHtmlModel(
    GenericHtmlModel[
        ExamGroupManagerInputModel, ExamGroupManagerMarkup, ExamGroupManagerStateModel
    ]
):
    def get_component_html_name(self) -> str:
        return "tim-exam-group-manager"

    def get_static_html(self) -> str:
        return "<div>Exam Group Manager</div>"


def reqs_handle() -> PluginReqs:
    return PluginReqs(
        js=["timExamGroupManager"],
        multihtml=True,
    )


register_html_routes(
    exam_group_manager_plugin, class_schema(ExamGroupManagerHtmlModel), reqs_handle
)
