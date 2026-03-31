from dataclasses import dataclass
from typing import Any, TypedDict, Union
from flask import request

from timApp.auth.sessioninfo import get_current_user_id
from timApp.tim_app import csrf
from timApp.util.flask.responsehelper import json_response
from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginAnswerResp,
    PluginReqs,
    EditorTab,
    PluginAnswerWeb,
    create_nontask_blueprint,
)
from timApp.modules.chattim.plugincore import (
    PluginCore,
    Result,
    ReqContext,
)

plugincore = PluginCore()


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
    # TODO: pitäisi varmaan muuttaa jotenkin tyyliin: define_ask_route(input: SomeDataClass) jne
    data = request.get_json()
    user_input = data.get("input")
    user_id = data.get("user_id")
    document_id = data.get("document_id")
    session_user_id = get_current_user_id()

    if user_id != session_user_id:
        pass
        # TODO onko tarkistus tarpeellinen, vai käytetäänkö vain session_user_id?

    resp = plugincore.chat_request(session_user_id, document_id, user_input)
    returnable = {"web": {"result": resp.value, "error": resp.error}}

    return json_response(returnable)


@dataclass()
class InstanceAttributes(TypedDict):
    model_name: str
    llm_mode: str
    max_tokens: str
    tim_documents: list[str]


@chattim.post("/save_instance")
def define_save_instance():
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}
    instance_attr: InstanceAttributes

    data = request.get_json()
    context = data.get("context")

    # TODO: kytke plugincoreen

    return json_response(result)
