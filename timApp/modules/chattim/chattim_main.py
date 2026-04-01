import os
import json
from dataclasses import dataclass
from typing import Any, TypedDict, Union
from flask import request, Response, stream_with_context
from typing import Any, TypedDict
from flask import Response, stream_with_context
from webargs.flaskparser import use_args
from tim_common.marshmallow_dataclass import class_schema

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


class ChatTimAskResponse(TypedDict, total=False):
    answer: str | None
    usage: int | None


@dataclass
class ChatTimAskParams:
    input: str
    user_id: str
    document_id: int


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
@use_args(class_schema(ChatTimAskParams)(), locations=("json",))
def define_ask_route(params: ChatTimAskParams):
    user_input = params.input
    user_id = params.user_id
    document_id = params.document_id
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
                    llm_mode: string,
                    max_tokens: int,
                    tim_paths: string,
                    }
    }
    :return: The usual with web.error if something went wrong
    """
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}
    instance_attr: InstanceAttributes

    data = request.get_json()
    cpanel_data = data.get("cpanel_data")
    user_id = data.get("user_id")
    document_id = data.get("document_id")

    try:
        instance_attr = InstanceAttributes(**cpanel_data)
    except (ValueError, TypeError):
        web["error"] = f"Server received malformed data: {data}"
        return json_response(result)

    session_user_id = get_current_user_id()

    # TODO onko tarkistus tarpeellinen, vai käytetäänkö vain session_user_id?
    if user_id != session_user_id:
        pass

    error_or_ok = plugincore.save_instance(session_user_id, document_id, instance_attr)
    if not error_or_ok.ok():
        web["error"] = error_or_ok.error
        return json_response(result)

    web["result"] = "Settings saved!"

    return json_response(result)


from .model import ModelRegistry, SUPPORTED_MODELS, ModelSpec, Message, GenerateOptions

# TODO: temporary
reg = ModelRegistry(SUPPORTED_MODELS)
model = reg.create(
    ModelSpec(
        provider="openai",
        model_id="gpt-4.1-nano",
        api_key=os.getenv("OPENAI_API_KEY", ""),
    )
)


@chattim.post("/askStream")
@use_args(class_schema(ChatTimAskParams)(), locations=("json",))
def define_ask_stream_route(params: ChatTimAskParams):
    user_input = params.input

    def generate():
        # TODO: temporary, use the plugincore
        stream = model.generate_stream(
            [Message(role="user", content=user_input)], GenerateOptions()
        )
        for msg in stream:
            print(msg)
            if msg.delta:
                yield json.dumps(ChatTimAskResponse(answer=msg.delta)) + "\n"
            if msg.usage:
                yield json.dumps(
                    ChatTimAskResponse(usage=msg.usage.total_tokens)
                ) + "\n"

    return Response(
        stream_with_context(generate()),
        mimetype="application/x-ndjson",
        headers={"Cache-Control": "no-cache", "X-Accel-Buffering": "no"},
    )
