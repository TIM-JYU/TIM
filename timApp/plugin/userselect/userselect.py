from dataclasses import dataclass

from flask import render_template_string

from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import GenericHtmlModel, create_nontask_blueprint, PluginReqs


@dataclass
class UserSelectInputModel:
    pass


@dataclass
class UserSelectMarkupModel(GenericMarkupModel):
    inputMinLength: int = 3
    autoSearchDelay: float = 0.0


@dataclass
class UserSelectStateModel:
    pass


@dataclass
class UserSelectHtmlModel(GenericHtmlModel[UserSelectInputModel, UserSelectMarkupModel, UserSelectStateModel]):
    def get_component_html_name(self) -> str:
        return 'user-selector'

    def get_static_html(self) -> str:
        return render_template_string(
            """
                <div>User selector</div>
            """
        )


def reqs_handler() -> PluginReqs:
    return {
        "js": ["userSelect"],
        "multihtml": True
    }


user_select_plugin = create_nontask_blueprint(
    __name__,
    'userSelect',
    UserSelectHtmlModel,
    reqs_handler,
)
