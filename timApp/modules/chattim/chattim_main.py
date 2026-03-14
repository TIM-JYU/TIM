from dataclasses import dataclass
from typing import Any

from timApp.tim_app import csrf
from timApp.util.flask.typedblueprint import TypedBlueprint
from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    GenericAnswerModel,
    register_plugin_app,
    launch_if_main,
    PluginAnswerResp,
    PluginReqs,
    EditorTab,
    PluginAnswerWeb,
    create_blueprint,
)


@dataclass
class ChatTimMarkupModel(GenericMarkupModel):
    pass


# TODO: make proper dataclasses
ChatTimInputModel = dict[str, Any]
ChatTimStateModel = dict[str, Any]


@dataclass
class ChatTimHtmlModel(
    GenericHtmlModel[ChatTimInputModel, ChatTimMarkupModel, ChatTimStateModel]
):
    def get_component_html_name(self) -> str:
        return "chattim-runner"


@dataclass
class ChatTimAnswerModel(
    GenericAnswerModel[ChatTimInputModel, ChatTimMarkupModel, ChatTimStateModel]
):
    pass


def answer(_args: ChatTimAnswerModel) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}
    web["result"] = "answer from the server"

    return result


def reqs() -> PluginReqs:
    templates = [
        """
``` {plugin="chattim" #taskidhere}
header: ChatTIM
```
""",
    ]
    editor_tabs: list[EditorTab] = [
        {
            "text": "Plugins",
            "items": [
                {
                    "text": "ChatTIM",
                    "items": [
                        {
                            "data": templates[0].strip(),
                            "text": "Chattim",
                            "expl": "Add a chatbot functionality",
                        },
                    ],
                },
            ],
        },
    ]
    result: PluginReqs = {
        "js": ["js/build/chattim.js"],
        "multihtml": True,
    }

    result["editor_tabs"] = editor_tabs
    return result


chattim = create_blueprint(
    __name__,
    "chattim",
    ChatTimHtmlModel,
    ChatTimAnswerModel,
    answer,
    reqs,
    csrf,
)
