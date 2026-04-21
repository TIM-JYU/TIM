from dataclasses import dataclass
from flask import request, Response, stream_with_context
from typing import Any, TypedDict, Callable
from webargs.flaskparser import use_args

from timApp.auth.accesshelper import verify_logged_in
from timApp.util.flask.requesthelper import RouteException
from tim_common.marshmallow_dataclass import class_schema

from timApp.auth.sessioninfo import get_current_user_id
from timApp.tim_app import csrf
from timApp.util.flask.responsehelper import json_response, to_json_str, ok_response
from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    PluginAnswerResp,
    PluginReqs,
    EditorTab,
    PluginAnswerWeb,
    Flask,
    Blueprint,
    jsonify,
    create_nontask_blueprint,
)
from timApp.modules.chattim.plugincore import (
    PluginCore,
    InstanceAttributes,
    ChatMessage,
)

plugincore = PluginCore()


@dataclass
class ChatTimMarkupModel(GenericMarkupModel):
    pass


# TODO: make proper dataclasses
ChatTimInputModel = dict[str, Any]
ChatTimStateModel = dict[str, Any]


@dataclass
class GenericParams:
    document_id: int


class ChatTimAskResponse(TypedDict, total=False):
    answer: str | None
    usage: int | None
    error: str | None


@dataclass
class ChatTimAskParams(GenericParams):
    input: str


@dataclass
class ChatTimSaveSettingsParams(GenericParams):
    control_panel_data: InstanceAttributes


@dataclass
class GetMessagesParams(GenericParams):
    amount: int
    timestamp_end_ms: int | None = None


@dataclass
class SaveAPIKeyParams:
    model: str  # TODO: rename to provider
    apikey: str
    alias: str


@dataclass
class ChatTIMGetSettingsResponse(TypedDict, total=False):
    result: InstanceAttributes
    error: str | None


def register_route(
    app: Flask | Blueprint,
    method: str,
    route: str,
    route_model: type | None,
    route_handler: Callable[..., Any],
) -> None:
    def to_response(r: Any) -> Response:
        # Allow handlers to define their own Flask Response
        if isinstance(r, Response):
            return r
        return jsonify(r)

    if route_model is None:

        @app.route(f"/{route}", methods=[method], endpoint=route)
        def define() -> Response:
            return to_response(route_handler())

        return

    @app.route(f"/{route}", methods=[method], endpoint=route)
    @use_args(class_schema(route_model)(), locations=("json",))
    def define(m: Any) -> Response:
        return to_response(route_handler(m))


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


def define_ask_route(params: ChatTimAskParams) -> ChatTimAskResponse:
    user_input = params.input
    document_id = params.document_id
    session_user_id = get_current_user_id()

    resp = plugincore.chat_request(session_user_id, document_id, user_input)

    response = ChatTimAskResponse(
        answer=resp.value,
        error=resp.error,
    )
    return response


def define_ask_stream_route(params: ChatTimAskParams) -> Response:
    user_input = params.input
    document_id = params.document_id

    session_user_id = get_current_user_id()

    def generate():
        resp = plugincore.chat_request_stream(session_user_id, document_id, user_input)
        if not resp.ok() or not resp.value:
            yield to_ndjson_str(ChatTimAskResponse(error=resp.error))
            return

        for chunk in resp.value:
            if chunk.delta:
                yield to_ndjson_str(ChatTimAskResponse(answer=chunk.delta))
            if chunk.usage:
                yield to_ndjson_str(ChatTimAskResponse(usage=chunk.usage.total_tokens))

    return Response(
        stream_with_context(generate()),
        mimetype="application/x-ndjson",
        headers={"X-Accel-Buffering": "no"},
    )


def define_get_settings(params: GenericParams) -> ChatTIMGetSettingsResponse:
    ret: ChatTIMGetSettingsResponse = {}
    document_id = params.document_id
    session_user_id = get_current_user_id()

    error_or_ok = plugincore.get_plugin_settings(session_user_id, document_id)
    if not error_or_ok.ok():
        ret["error"] = error_or_ok.error
        return ret

    ret["result"] = error_or_ok.value
    return ret


def define_save_settings(params: ChatTimSaveSettingsParams) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}

    panel_data = params.control_panel_data
    document_id = params.document_id
    session_user_id = get_current_user_id()

    error_or_ok = plugincore.save_instance(session_user_id, document_id, panel_data)
    if not error_or_ok.ok():
        web["error"] = error_or_ok.error
        return result

    web["result"] = "Settings saved!"
    return result


def define_get_messages(params: GetMessagesParams) -> dict:
    user_id = str(get_current_user_id())
    document_id = str(params.document_id)
    amount = params.amount
    ts_end = params.timestamp_end_ms or ChatMessage.ts_ms()

    chat_messages = plugincore.get_messages_tw(user_id, document_id, 0, ts_end, amount)
    messages = [
        {"content": m.content, "role": m.role, "timestamp_ms": m.timestamp}
        for m in chat_messages
        if m.role in ("user", "assistant")
    ]
    return {"messages": messages}


def define_save_api_key(params: SaveAPIKeyParams) -> Response:
    verify_logged_in()

    provider = params.model
    key = params.apikey
    alias = params.alias

    valid = plugincore.validate_api_key(provider, key)

    if valid:
        return ok_response()
    else:
        raise RouteException(description="API Key is invalid.")

    # TODO avaimen tallennus validoimisen jälkeen, jos avain jo niin palautetaan tieto siitä


def define_get_providers() -> list[str]:
    response = plugincore.get_supported_providers()
    return response


def to_ndjson_str(json_data: Any) -> str:
    """Return a newline delimited JSON string.

    :param json_data: The data to be converted.
    :return: A string representation of the JSON data ending in a newline.
    """
    return to_json_str(json_data) + "\n"


chattim = create_nontask_blueprint(
    name=__name__,
    plugin_name="chattim",
    html_model=ChatTimHtmlModel,
    reqs_handler=reqs,
    csrf=csrf,
)

register_route(chattim, "post", "ask", ChatTimAskParams, define_ask_route)
register_route(chattim, "post", "askStream", ChatTimAskParams, define_ask_stream_route)
register_route(chattim, "post", "getSettings", GenericParams, define_get_settings)
register_route(
    chattim, "post", "saveSettings", ChatTimSaveSettingsParams, define_save_settings
)
register_route(chattim, "post", "getMessages", GetMessagesParams, define_get_messages)
register_route(chattim, "post", "validate_api", SaveAPIKeyParams, define_save_api_key)
register_route(chattim, "get", "get_providers", None, define_get_providers)
