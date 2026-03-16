from dataclasses import dataclass
from typing import Any
from flask import request

from timApp.tim_app import csrf
from timApp.util.flask.responsehelper import json_response
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
    create_nontask_blueprint,
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


chattim = create_nontask_blueprint(
    name=__name__,
    plugin_name="chattim",
    html_model=ChatTimHtmlModel,
    reqs_handler=reqs,
    csrf=csrf,
)


@chattim.post("/ask")
def define_ask_route():
    web: PluginAnswerWeb = {"result": "hello from server"}
    result: PluginAnswerResp = {"web": web}

    # TODO: pitäisi varmaan muuttaa jotenkin tyyliin: define_ask_route(input: SomeDataClass) jne
    data = request.get_json()
    user_input = data.get("input")
    user_id = data.get("user_id")

    # TODO: kytke plugincoreen

    return json_response(result)
