from dataclasses import dataclass
from typing import Any, TypedDict, Union
from flask import request

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


def read_into_reqcontext(data: Any) -> Union[ReqContext, str]:
    """
    Expects { user_input, user_id, document_id }
    :param data:
    :return: Either reponse or error message
    """
    if data is None:
        return "Missing json body"

    try:
        resp = ReqContext(**data)
        return resp
    except TypeError as e:
        return f"Invalid data: {e}"
    except Exception as e:
        return f"Unexpected error: {e}"


@chattim.post("/ask")
def define_ask_route():
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}

    context = request.get_json().get("context")
    reqcontext_or = read_into_reqcontext(context)

    if not isinstance(reqcontext_or, ReqContext):
        web["error"] = reqcontext_or
        return json_response(result)

    val_or_err: Result[str, str] = plugincore.chat_request(reqcontext_or)
    if not val_or_err.ok:
        web["error"] = val_or_err.error
        return json_response(result)

    web["result"] = val_or_err.value
    return json_response(result)


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
    req_context_or = read_into_reqcontext(context)

    if not isinstance(req_context_or, ReqContext):
        web = {"error": req_context_or}
        return json_response(result)

    # TODO: kytke plugincoreen

    return json_response(result)
