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
from timApp.modules.chattim.plugincore import PluginCore, InstanceAttributes

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

    # TODO onko tarkistus tarpeellinen, vai käytetäänkö vain session_user_id?
    if user_id != session_user_id:
        pass

    resp = plugincore.chat_request(session_user_id, document_id, user_input)
    returnable = {"web": {"result": resp.value, "error": resp.error}}

    return json_response(returnable)


@chattim.post("/settings_save")
def define_save_settings():
    """
    Saves settings sent from control panel. Received json should be:
    { user_id: int,
      document_id: int,
      cpanel_data: {
                    model_id: string,
                    mode: string,
                    max_tokens: int,
                    tim_paths: string,
                    }
    }
    tim_paths is list of paths separated by newline
    :return: The usual with web.error if something went wrong
    """
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}
    instance_attr: InstanceAttributes

    data = request.get_json()
    cpanel_data = data.get("cpanel_data")
    user_id = data.get("user_id")
    document_id = data.get("document_id")

    instance_attr = InstanceAttributes(**cpanel_data)
    session_user_id = get_current_user_id()

    # TODO onko tarkistus tarpeellinen, vai käytetäänkö vain session_user_id?
    if user_id != session_user_id:
        pass

    plugincore.save_instance(session_user_id, document_id, instance_attr)

    return json_response(result)
