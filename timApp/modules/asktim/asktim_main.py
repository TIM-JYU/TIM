from dataclasses import dataclass
from flask import request, Response, stream_with_context
from typing import (
    TypedDict,
    Callable,
    Iterator,
    cast,
    Literal,
    TypeAlias,
)
from webargs.flaskparser import use_args
import json

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
    PluginReqs,
    EditorTab,
    PluginAnswerWeb,
    Flask,
    Blueprint,
    jsonify,
    create_nontask_blueprint,
)
from .plugincore import (
    PluginCore,
    InstanceAttributes,
    APIKey,
    UserData,
    InstanceSettingsData,
)
from timApp.plugin.plugin import Plugin
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, ViewRoute
from timApp.user.user import User

plugincore = PluginCore()

RouteReturn: TypeAlias = Response | list | object


@dataclass
class AskTimMarkupModel(GenericMarkupModel):
    welcomeText: str = "Welcome to use TIM's helper chatbot!"
    apiAlias: str = ""
    defaultWindowSize: Literal["sm", "md", "lg", "xs"] = "md"
    blockContent: str = ""
    previewVisible: bool = False
    startMinimized: bool = False
    startBottomRight: bool = False


@dataclass
class GenericParams:
    document_id: int


@dataclass
class GetRightsParams(GenericParams):
    pass


@dataclass
class SaveUserPolicyParams(GenericParams):
    user_data: UserData


@dataclass
class DeletePluginParams(GenericParams):
    par_id: str


def get_rights(params: GetRightsParams) -> dict:
    doc = get_doc_or_abort(params.document_id)
    # verify_teacher_access returns the access object if teacher, None if not
    teacher_access = verify_teacher_access(doc, require=False)

    return {
        "is_teacher": teacher_access is not None,
    }


class AskTimAskResponse(TypedDict, total=False):
    answer: str | None
    citations: list[str] | None
    error: str | None


@dataclass
class AskTimAskParams(GenericParams):
    input: str


@dataclass
class GetModelsParams:
    public_key: str


@dataclass
class AskTimSaveSettingsParams(GenericParams):
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
class AskTimGetSettingsResponse(TypedDict, total=False):
    result: InstanceAttributes
    error: str | None


@dataclass
class AskTimHtmlModel(GenericHtmlModel[dict, AskTimMarkupModel, dict]):
    def get_component_html_name(self) -> str:
        return "asktim-runner"


class AskTimAnswerWeb(PluginAnswerWeb, total=False):
    availableModels: list[dict[str, str]]
    selectedModel: str


# Leave "welcomeText" empty to use default localized welcome text
def reqs() -> PluginReqs:
    templates = [
        """
``` {plugin="asktim" #taskidhere}
header: AskTIM
welcomeText: ""
apiAlias: ""
defaultWindowSize: "md" # [sm, md, lg, xs]
blockContent: ""
previewVisible: false
startMinimized: false
startBottomRight: false
```
""",
    ]
    editor_tabs: list[EditorTab] = [
        {
            "text": "Plugins",
            "items": [
                {
                    "text": "AskTim",
                    "items": [
                        {
                            "data": templates[0].strip(),
                            "text": "LLM assistant",
                            "expl": "Add a chatbot functionality",
                        },
                    ],
                },
            ],
        },
    ]
    result: PluginReqs = PluginReqs(
        js=["js/build/asktim.js"],
        multihtml=True,
    )

    result["editor_tabs"] = editor_tabs
    return result


asktim = create_nontask_blueprint(
    name=__name__,
    plugin_name="asktim",
    html_model=AskTimHtmlModel,
    reqs_handler=reqs,
    csrf=csrf,
)


def register_route(
    app: Flask | Blueprint,
    method: str,
    route: str,
    route_model: type | None,
    route_handler: Callable[..., RouteReturn],
) -> None:
    def to_response(r: RouteReturn) -> Response:
        # Allow handlers to define their own Flask Response
        if isinstance(r, Response):
            return r
        return jsonify(r)

    if route_model is None:

        @app.route(f"/{route}", methods=[method], endpoint=route)
        def handler() -> Response:
            try:
                return to_response(route_handler())
            except RouteException as e:
                raise e
            except Exception as e:
                raise RouteException(str(e)) from e

        return

    @app.route(f"/{route}", methods=[method], endpoint=route)
    @use_args(class_schema(route_model)(), locations=("json",))
    def handler_args(m: object) -> Response:
        try:
            return to_response(route_handler(m))
        except RouteException as e:
            raise e
        except Exception as e:
            raise RouteException(str(e)) from e


def ask_route(params: AskTimAskParams) -> AskTimAskResponse:
    user_input = params.input
    document_id = params.document_id
    session_user_id = get_current_user_id()
    check_view_rights(user_id=session_user_id, document_id=document_id)

    document_has_asktim_plugin(document_id, session_user_id)

    resp = plugincore.chat_request(session_user_id, document_id, user_input)
    if resp.ok():
        value: tuple[str, list[str] | None] = resp.value or ("", None)
        message, context = value
        return AskTimAskResponse(
            answer=message,
            citations=context,
        )
    return AskTimAskResponse(error=resp.error)


def ask_stream_route(params: AskTimAskParams) -> Response:
    user_input = params.input
    document_id = params.document_id
    session_user_id = get_current_user_id()
    check_view_rights(user_id=session_user_id, document_id=document_id)

    document_has_asktim_plugin(document_id, session_user_id)

    def generate() -> Iterator[str]:
        resp = plugincore.chat_request_stream(session_user_id, document_id, user_input)
        if not resp.ok() or not resp.value:
            yield to_ndjson_str(AskTimAskResponse(error=resp.error))
            return

        citations: list[str] | None = None
        disconnected: bool = False

        try:
            stream, context = resp.value
            citations = context
            for chunk in stream:
                yield to_ndjson_str(AskTimAskResponse(answer=chunk))
        except GeneratorExit:
            disconnected = True
            return
        except Exception as e:
            yield to_ndjson_str(AskTimAskResponse(error=str(e)))
            return
        finally:
            if not disconnected:
                yield to_ndjson_str(AskTimAskResponse(citations=citations))

    return Response(
        stream_with_context(generate()),
        mimetype="application/x-ndjson",
        headers={"X-Accel-Buffering": "no"},
    )


def get_settings(params: GenericParams) -> AskTimGetSettingsResponse:
    ret: AskTimGetSettingsResponse = {}
    document_id = params.document_id
    session_user_id = get_current_user_id()

    check_view_rights(user_id=session_user_id, document_id=document_id)
    # document_has_asktim_plugin(document_id, session_user_id)

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
    check_view_rights(user_id=session_user_id, document_id=document_id)

    document_has_asktim_plugin(document_id, session_user_id)

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
    check_view_rights(user_id=session_user_id, document_id=document_id)
    operation_result: dict = {"error": "", "result": ""}

    document_has_asktim_plugin(document_id, session_user_id)

    try:
        result_msg = plugincore.save_user_policy(
            session_user_id, document_id, user_data
        )
        operation_result["result"] = result_msg
    except (LookupError, ValueError) as e:
        operation_result["error"] = str(e)

    return operation_result


def save_settings(params: AskTimSaveSettingsParams) -> AskTimGetSettingsResponse:
    res: AskTimGetSettingsResponse = {}

    panel_data = params.control_panel_settings
    document_id = params.document_id
    session_user_id = get_current_user_id()
    check_view_rights(user_id=session_user_id, document_id=document_id)

    document_has_asktim_plugin(document_id, session_user_id)

    save_result = plugincore.save_instance(session_user_id, document_id, panel_data)
    if not save_result.ok() or save_result.value is None:
        res["error"] = save_result.error or ""
        return res

    res["result"] = save_result.value or InstanceSettingsData.default()
    return res


def get_messages(params: GetMessagesParams) -> dict:
    user_id = get_current_user_id()
    document_id = params.document_id
    check_view_rights(user_id=user_id, document_id=document_id)
    # document_has_asktim_plugin(document_id, user_id)

    amount = params.amount
    ts_end = params.timestamp_end_ms
    messages = plugincore.get_messages_ui(user_id, document_id, ts_end, amount)
    return {"messages": messages}


def clear_messages(params: GenericParams) -> Response:
    user_id = get_current_user_id()
    document_id = params.document_id
    check_view_rights(user_id=user_id, document_id=document_id)
    document_has_asktim_plugin(document_id, user_id)

    plugincore.clear_history(str(user_id), str(document_id))
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


@asktim.post("/removeGroupRight/<int:group_id>")
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


def to_ndjson_str(json_data: object) -> str:
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


def delete_plugin(params: DeletePluginParams) -> Response:
    session_user_id = get_current_user_id()
    document_id = params.document_id
    par_id = params.par_id

    check_view_rights(user_id=session_user_id, document_id=document_id)
    document_has_asktim_plugin(document_id, session_user_id)

    result = plugincore.delete_instance(session_user_id, document_id, par_id)

    if result.error is not None:
        raise RouteException(description=result.error or "")
    return ok_response()


def document_has_asktim_plugin(document_id: int, caller_id: int) -> None:
    document = plugincore.get_tim_document_by_id(document_id)
    if document is None:
        raise LookupError(f"Document with id {document_id} not found")

    user = User.get_by_id(caller_id)
    if user is None:
        raise LookupError(f"User with id {caller_id} not found")

    view_ctx = ViewContext(ViewRoute.View, preview=False)
    user_ctx = UserContext(user=user, logged_user=user)

    for paragraph in document.get_paragraphs():
        is_plugin = paragraph.is_plugin()
        if is_plugin:
            plugin = Plugin.from_paragraph(paragraph, view_ctx, user_ctx)
            if plugin.type == "asktim":
                return

    raise RouteException(f"No AskTim plugin found in the document {document_id}")


def check_view_rights(document_id: int, user_id: int) -> None:
    """Check if the user has view rights to the document
    :param document_id:id of the document to check
    :param user_id:id of the user to check"""
    user = User.get_by_id(user_id)
    doc = get_doc_or_abort(document_id)
    if user is None or not user.has_view_access(doc):
        raise RouteException(f"You dont have access to this document")


register_route(asktim, "post", "ask", AskTimAskParams, ask_route)
register_route(asktim, "post", "askStream", AskTimAskParams, ask_stream_route)
register_route(asktim, "post", "getSettings", GenericParams, get_settings)
register_route(asktim, "post", "saveSettings", AskTimSaveSettingsParams, save_settings)
register_route(asktim, "post", "getMessages", GetMessagesParams, get_messages)
register_route(asktim, "post", "clearMessages", GenericParams, clear_messages)
register_route(asktim, "post", "deletePlugin", DeletePluginParams, delete_plugin)
register_route(asktim, "post", "validateApi", APIKeyParams, save_api_key)
register_route(asktim, "get", "getProviders", None, get_providers)
register_route(asktim, "post", "getUserPolicyData", GenericParams, get_user_data)
register_route(asktim, "post", "saveUserPolicy", SaveUserPolicyParams, save_user_policy)
register_route(
    asktim,
    "post",
    "addApiKeyPermissions",
    APIKeyParams,
    add_api_key_permissions,
)
register_route(asktim, "post", "getRights", GetRightsParams, get_rights)
register_route(asktim, "get", "getExistingKeys", None, get_existing_keys)
register_route(asktim, "delete", "deleteKey", APIKeyParams, delete_existing_key)
register_route(asktim, "post", "getModels", GetModelsParams, get_models)
