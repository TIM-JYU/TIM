from dataclasses import dataclass
from flask import request, Response, stream_with_context
from typing import Any, TypedDict, Callable
from webargs.flaskparser import use_args
from flask_babel import gettext

from timApp.auth.accesshelper import (
    verify_logged_in,
    verify_teacher_access,
    get_doc_or_abort,
)
from timApp.util.flask.requesthelper import RouteException
from tim_common.marshmallow_dataclass import class_schema

from timApp.auth.sessioninfo import get_current_user_id, get_current_user_name
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
)
from .model import ModelError

plugincore = PluginCore()


@dataclass
class ChatTimMarkupModel(GenericMarkupModel):
    welcomeText: str = "Welcome to use TIM's helper chatbot!"


# TODO: make proper dataclasses
ChatTimInputModel = dict[str, Any]
ChatTimStateModel = dict[str, Any]


@dataclass
class GenericParams:
    document_id: int


@dataclass
class GetRightsParams(GenericParams):
    pass


def get_rights(params: GetRightsParams) -> dict:
    doc = get_doc_or_abort(params.document_id)
    print(doc)
    # verify_teacher_access returns the access object if teacher, None if not
    teacher_access = verify_teacher_access(doc, require=False)

    return {
        "is_teacher": teacher_access is not None,
    }


class ChatTimAskResponse(TypedDict, total=False):
    answer: str | None
    usage: int | None
    error: str | None


@dataclass
class ChatTimAskParams(GenericParams):
    input: str


@dataclass
class ChatTimSaveSettingsParams(GenericParams):
    control_panel_settings: InstanceAttributes


@dataclass
class GetMessagesParams(GenericParams):
    amount: int
    timestamp_end_ms: int | None = None


@dataclass
class APIKeyParams:
    provider: str
    apikey: str
    alias: str
    groups: list[str] | None = None


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


# Leave "welcomeText" empty to use default localized welcome text
def reqs() -> PluginReqs:
    templates = [
        """
``` {plugin="chattim" #taskidhere}
header: ChatTIM
welcomeText: ""  
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


def ask_route(params: ChatTimAskParams) -> ChatTimAskResponse:
    user_input = params.input
    document_id = params.document_id
    session_user_id = get_current_user_id()

    resp = plugincore.chat_request(session_user_id, document_id, user_input)

    response = ChatTimAskResponse(
        answer=resp.value,
        error=resp.error,
    )
    return response


def ask_stream_route(params: ChatTimAskParams) -> Response:
    user_input = params.input
    document_id = params.document_id

    session_user_id = get_current_user_id()

    def generate():
        resp = plugincore.chat_request_stream(session_user_id, document_id, user_input)
        if not resp.ok() or not resp.value:
            yield to_ndjson_str(ChatTimAskResponse(error=resp.error))
            return

        try:
            for chunk in resp.value:
                if chunk.delta:
                    yield to_ndjson_str(ChatTimAskResponse(answer=chunk.delta))
                if chunk.usage:
                    yield to_ndjson_str(
                        ChatTimAskResponse(usage=chunk.usage.total_tokens)
                    )
        except ModelError as e:
            yield to_ndjson_str(ChatTimAskResponse(error=e.text()))
            return
        except Exception as e:
            yield to_ndjson_str(ChatTimAskResponse(error=str(e)))
            return

    return Response(
        stream_with_context(generate()),
        mimetype="application/x-ndjson",
        headers={"X-Accel-Buffering": "no"},
    )


def get_settings(params: GenericParams) -> ChatTIMGetSettingsResponse:
    ret: ChatTIMGetSettingsResponse = {}
    document_id = params.document_id
    session_user_id = get_current_user_id()

    error_or_ok = plugincore.get_plugin_settings(session_user_id, document_id)
    if not error_or_ok.ok():
        ret["error"] = error_or_ok.error
        return ret

    ret["result"] = error_or_ok.value
    return ret


def save_settings(params: ChatTimSaveSettingsParams) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}

    panel_data = params.control_panel_settings
    document_id = params.document_id
    session_user_id = get_current_user_id()

    error_or_ok = plugincore.save_instance(session_user_id, document_id, panel_data)
    if not error_or_ok.ok():
        web["error"] = error_or_ok.error
        return result

    web["result"] = "Settings saved!"
    return result


def get_messages(params: GetMessagesParams) -> dict:
    user_id = str(get_current_user_id())
    document_id = str(params.document_id)
    amount = params.amount
    ts_end = params.timestamp_end_ms

    chat_messages = plugincore.get_messages_tw(
        user_id, document_id, None, ts_end, amount
    )
    messages = [
        {"content": m.content, "role": m.role, "timestamp_ms": m.timestamp}
        for m in chat_messages
        if m.role in ("user", "assistant")
    ]
    return {"messages": messages}


def save_api_key(params: APIKeyParams) -> Response:
    verify_logged_in()

    provider = params.provider
    key = params.apikey
    alias = params.alias
    userid = get_current_user_id()
    username = get_current_user_name()
    groups = params.groups or [username]

    valid = plugincore.validate_api_key(provider, key)

    if valid:
        try:
            plugincore.add_api_key(userid, provider, alias, key, group_names=groups)
        except Exception as e:
            raise RouteException(description=str(e))

        return ok_response()
    else:
        raise RouteException(description="API Key is invalid.")


def save_api_key_permissions(params: APIKeyParams) -> Response:
    verify_logged_in()
    user_id = get_current_user_id()
    alias = params.alias
    groups = params.groups or []
    try:
        plugincore.update_api_key_permissions(user_id, alias, groups)
    except Exception as e:
        raise RouteException(description=str(e))
    return ok_response()


def get_providers() -> list[str]:
    response = plugincore.get_supported_providers()
    return response


def get_existing_keys() -> list[str]:
    verify_logged_in()
    userid = get_current_user_id()
    api_keys = plugincore.get_user_api_keys(userid)

    hidden_keys = []
    for key in api_keys:
        alias, provider, api_key, groups = key
        hidden_key = api_key[:6] + "..." + api_key[-4:]
        hidden_keys.append(
            {
                "provider": provider,
                "APIkey": hidden_key,
                "alias": alias,
                "groupNames": groups,
            }
        )

    return hidden_keys


def delete_existing_key(key: APIKeyParams) -> Response:
    verify_logged_in()
    alias = key.alias
    userid = get_current_user_id()

    try:
        plugincore.delete_api_key(userid, alias)
    except Exception:
        raise RouteException(description="No API-keys found")

    return ok_response()


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

register_route(chattim, "post", "ask", ChatTimAskParams, ask_route)
register_route(chattim, "post", "askStream", ChatTimAskParams, ask_stream_route)
register_route(chattim, "post", "getSettings", GenericParams, get_settings)
register_route(
    chattim, "post", "saveSettings", ChatTimSaveSettingsParams, save_settings
)
register_route(chattim, "post", "getMessages", GetMessagesParams, get_messages)
register_route(chattim, "post", "validateApi", APIKeyParams, save_api_key)
register_route(chattim, "get", "getProviders", None, get_providers)
register_route(
    chattim,
    "post",
    "saveApiKeyPermissions",
    APIKeyParams,
    save_api_key_permissions,
)
register_route(chattim, "post", "getRights", GetRightsParams, get_rights)
register_route(chattim, "get", "getExistingKeys", None, get_existing_keys)
register_route(chattim, "delete", "deleteKey", APIKeyParams, delete_existing_key)
