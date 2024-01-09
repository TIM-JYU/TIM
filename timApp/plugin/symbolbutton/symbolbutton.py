from dataclasses import dataclass

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

symbolbutton_plugin = TypedBlueprint(
    "symbolbutton_plugin", __name__, url_prefix="/symbolbutton"
)


@dataclass
class MdButtonType:
    text: str
    data: str
    expl: str
    type: str


@dataclass
class SymbolButtonMarkup(GenericMarkupModel):
    buttons: str | Missing = missing
    mdButtons: list[MdButtonType] | Missing = missing
    mini: bool | Missing = missing
    float: bool | Missing = missing


@dataclass
class SymbolButtonStateModel:
    pass


@dataclass
class SymbolButtonInputModel:
    pass


@dataclass
class SymbolButtonHtmlModel(
    GenericHtmlModel[SymbolButtonInputModel, SymbolButtonMarkup, SymbolButtonStateModel]
):
    def get_component_html_name(self) -> str:
        return "symbolbutton-runner"

    def get_static_html(self) -> str:
        return """
            <div>SymbolButton</div>
        """


def reqs_handle() -> PluginReqs:
    return {
        "js": ["symbolbutton"],
        "multihtml": True,
    }


register_html_routes(
    symbolbutton_plugin, class_schema(SymbolButtonHtmlModel), reqs_handle
)
