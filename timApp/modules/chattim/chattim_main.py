from dataclasses import dataclass
from flask import request, Response, stream_with_context
from typing import Any, TypedDict, Callable, Iterator, cast, Literal
from webargs.flaskparser import use_args
import json
from flask_babel import gettext

from timApp.auth.accesshelper import (
    verify_logged_in,
    verify_teacher_access,
    get_doc_or_abort,
)
from timApp.util.flask.requesthelper import RouteException
from tim_common.marshmallow_dataclass import class_schema
from timApp.user.user import User
from timApp.document.document import Document
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
    APIKey,
    UserData,
    InstanceSettingsData,
)
from .model import ModelError

plugincore = PluginCore()


@dataclass
class ChatTimMarkupModel(GenericMarkupModel):
    welcomeText: str = "Welcome to use TIM's helper chatbot!"
    apiAlias: str = ""
    defaultWindowSize: Literal["sm", "md", "lg", "xs"] = "md"
    blockContent: str = ""


# TODO: make proper dataclasses
ChatTimInputModel = dict[str, Any]
ChatTimStateModel = dict[str, Any]


@dataclass
class GenericParams:
    document_id: int


@dataclass
class GetRightsParams(GenericParams):
    pass


@dataclass
class SaveUserPolicyParams(GenericParams):
    user_data: UserData


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
    citations: list[str] | None
    error: str | None


@dataclass
class ChatTimAskParams(GenericParams):
    input: str


@dataclass
class GetModelsParams:
    public_key: str


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
    paths: list[str] | None = None


@dataclass
class ChatTIMGetSettingsResponse(TypedDict, total=False):
    result: InstanceAttributes
    error: str | None


@dataclass
class ChatTimHtmlModel(
    GenericHtmlModel[ChatTimInputModel, ChatTimMarkupModel, ChatTimStateModel]
):
    def get_component_html_name(self) -> str:
        return "chattim-runner"


class ChatTimAnswerWeb(PluginAnswerWeb, total=False):
    availableModels: list[dict[str, str]]
    selectedModel: str


# Leave "welcomeText" empty to use default localized welcome text
def reqs() -> PluginReqs:
    templates = [
        """
``` {plugin="chattim" #taskidhere}
header: ChatTIM
welcomeText: ""  
apiAlias: ""
defaultWindowSize: "md" # [sm, md, lg, xs]
blockContent: ""
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
    result: PluginReqs = PluginReqs(
        js=["js/build/chattim.js"],
        multihtml=True,
    )

    result["editor_tabs"] = editor_tabs
    return result


chattim = create_nontask_blueprint(
    name=__name__,
    plugin_name="chattim",
    html_model=ChatTimHtmlModel,
    reqs_handler=reqs,
    csrf=csrf,
)


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
        def handler() -> Response:
            return to_response(route_handler())

        return

    @app.route(f"/{route}", methods=[method], endpoint=route)
    @use_args(class_schema(route_model)(), locations=("json",))
    def handler_args(m: Any) -> Response:
        return to_response(route_handler(m))


def ask_route(params: ChatTimAskParams) -> ChatTimAskResponse:
    user_input = params.input
    document_id = params.document_id
    session_user_id = get_current_user_id()
    user = User.get_by_id(session_user_id)
    doc = get_doc_or_abort(document_id)
    # check if user has access to document where plugin is added

    if not user.has_view_access(doc):
        return ChatTimAskResponse(error="You dont have access to this document")
    resp = plugincore.chat_request(session_user_id, document_id, user_input)
    if resp.ok():
        value: tuple[str, list[str] | None] = resp.value or ("", None)
        message, context = value
        return ChatTimAskResponse(
            answer=message,
            citations=context,
        )
    return ChatTimAskResponse(error=resp.error)


def ask_stream_route(params: ChatTimAskParams) -> Response:
    user_input = params.input
    document_id = params.document_id

    session_user_id = get_current_user_id()
    user = User.get_by_id(session_user_id)
    doc = get_doc_or_abort(document_id)

    def generate() -> Iterator[str]:
        # check if user has access to document where plugin is added
        if not user.has_view_access(doc):
            yield to_ndjson_str(
                ChatTimAskResponse(error="You dont have access to this document")
            )
            return

        resp = plugincore.chat_request_stream(session_user_id, document_id, user_input)
        if not resp.ok() or not resp.value:
            yield to_ndjson_str(ChatTimAskResponse(error=resp.error))
            return

        citations: list[str] | None = None

        try:
            stream, context = resp.value
            citations = context
            for chunk in stream:
                yield to_ndjson_str(ChatTimAskResponse(answer=chunk))
        except ModelError as e:
            error = f"{e.text()} {str(e.cause)}"
            yield to_ndjson_str(ChatTimAskResponse(error=error))
            return
        except Exception as e:
            yield to_ndjson_str(ChatTimAskResponse(error=str(e)))
            return
        finally:
            yield to_ndjson_str(ChatTimAskResponse(citations=citations))

    return Response(
        stream_with_context(generate()),
        mimetype="application/x-ndjson",
        headers={"X-Accel-Buffering": "no"},
    )


def get_settings(params: GenericParams) -> ChatTIMGetSettingsResponse:
    ret: ChatTIMGetSettingsResponse = {}
    document_id = params.document_id
    session_user_id = get_current_user_id()

    get_result = plugincore.get_plugin_settings(session_user_id, document_id)
    if not get_result.ok():
        ret["error"] = get_result.error
        return ret

    ret["result"] = get_result.value or InstanceSettingsData.default()

    return ret


def get_user_data(params: GenericParams) -> dict:
    """
    Gets user data (policy and usage) for all users that have chatted with the instance
    :param params:
    :return:
    """
    document_id = params.document_id
    session_user_id = get_current_user_id()
    ret: dict = {}

    try:
        data_list = plugincore.get_user_data(session_user_id, document_id)
        ret["result"] = data_list
    except LookupError as e:
        ret["error"] = e

    return ret


def save_user_policy(params: SaveUserPolicyParams) -> dict:
    document_id = params.document_id
    user_data = params.user_data
    session_user_id = get_current_user_id()
    operation_result: dict = {"error": "", "result": ""}

    try:
        result_msg = plugincore.save_user_policy(
            session_user_id, document_id, user_data
        )
        operation_result["result"] = result_msg
    except (LookupError, ValueError) as e:
        operation_result["error"] = str(e)

    return operation_result


def save_settings(params: ChatTimSaveSettingsParams) -> ChatTIMGetSettingsResponse:
    res: ChatTIMGetSettingsResponse = {}

    panel_data = params.control_panel_settings
    document_id = params.document_id
    session_user_id = get_current_user_id()

    save_result = plugincore.save_instance(session_user_id, document_id, panel_data)
    if not save_result.ok() or save_result.value is None:
        res["error"] = save_result.error or ""
        return res

    res["result"] = save_result.value or InstanceSettingsData.default()
    return res


def get_messages(params: GetMessagesParams) -> dict:
    user_id = get_current_user_id()
    document_id = params.document_id
    amount = params.amount
    ts_end = params.timestamp_end_ms
    messages = plugincore.get_messages_ui(user_id, document_id, ts_end, amount)
    return {"messages": messages}


def clear_messages(params: GenericParams) -> Response:
    user_id = str(get_current_user_id())
    document_id = str(params.document_id)

    plugincore.clear_history(user_id, document_id)
    return ok_response()


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
            api_key = plugincore.add_api_key(
                userid, provider, alias, key, group_names=groups
            )
            return json_response(_api_key_to_dict(api_key))
        except Exception as e:
            raise RouteException(description=str(e))
    else:
        raise RouteException(description="API Key is invalid.")


def add_api_key_permissions(params: APIKeyParams) -> Response:
    verify_logged_in()
    user_id = get_current_user_id()
    alias = params.alias
    groups = params.groups or []
    paths = params.paths or []
    try:
        user_groups, item_paths = plugincore.update_api_key_permissions(
            user_id, alias, groups, paths
        )
    except Exception as e:
        raise RouteException(description=str(e))
    res = {
        "groups": user_groups,
        "paths": item_paths,
    }
    return json_response(res)


@chattim.post("/removeGroupRight/<int:group_id>")
def remove_key_group_right(group_id: int) -> Response:
    user_id = get_current_user_id()
    data = json.loads(request.data)
    try:
        public_key = data["public_key"]
        plugincore.remove_api_key_group(user_id, public_key, group_id)
    except KeyError:
        raise RouteException(description="Bad request.")
    return ok_response()


def get_providers() -> list[str]:
    response = plugincore.get_supported_providers()
    return cast(list[str], response)


def get_existing_keys() -> list[dict]:
    verify_logged_in()
    userid = get_current_user_id()
    api_keys = plugincore.get_user_api_keys(userid)

    hidden_keys = []
    for key in api_keys:
        hidden_keys.append(_api_key_to_dict(key))
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


def _api_key_to_dict(key: APIKey) -> dict:
    alias, provider, api_key, groups, paths = key
    hidden_key = api_key[:6] + "..." + api_key[-4:]
    return {
        "provider": provider,
        "APIkey": hidden_key,
        "alias": alias,
        "groups": groups,
        "itemPaths": paths,
    }


def get_models(params: GetModelsParams) -> dict:
    user_id = get_current_user_id()
    try:
        provider, api_key = plugincore.try_access_api_key(user_id, params.public_key)
    except Exception as e:
        return {"error": str(e), "models": []}
    models = plugincore.get_models(provider, api_key)
    return {"models": models}


register_route(chattim, "post", "ask", ChatTimAskParams, ask_route)
register_route(chattim, "post", "askStream", ChatTimAskParams, ask_stream_route)
register_route(chattim, "post", "getSettings", GenericParams, get_settings)
register_route(
    chattim, "post", "saveSettings", ChatTimSaveSettingsParams, save_settings
)
register_route(chattim, "post", "getMessages", GetMessagesParams, get_messages)
register_route(chattim, "post", "clearMessages", GenericParams, clear_messages)
register_route(chattim, "post", "validateApi", APIKeyParams, save_api_key)
register_route(chattim, "get", "getProviders", None, get_providers)
register_route(chattim, "post", "getUserPolicyData", GenericParams, get_user_data)
register_route(
    chattim, "post", "saveUserPolicy", SaveUserPolicyParams, save_user_policy
)
register_route(
    chattim,
    "post",
    "addApiKeyPermissions",
    APIKeyParams,
    add_api_key_permissions,
)
register_route(chattim, "post", "getRights", GetRightsParams, get_rights)
register_route(chattim, "get", "getExistingKeys", None, get_existing_keys)
register_route(chattim, "delete", "deleteKey", APIKeyParams, delete_existing_key)
register_route(chattim, "post", "getModels", GetModelsParams, get_models)
