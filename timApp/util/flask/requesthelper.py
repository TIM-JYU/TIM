import itertools
import json
import os
import pprint
import time
import warnings
from dataclasses import is_dataclass, dataclass
from typing import TypeVar, Callable, Any
from urllib.parse import urlparse

import requests
from flask import Request, current_app, g, Response, has_request_context
from flask import request
from marshmallow import ValidationError, Schema
from webargs.flaskparser import use_args
from werkzeug.exceptions import HTTPException
from werkzeug.user_agent import UserAgent

from timApp.auth.sessioninfo import get_current_user_name, get_current_session_id
from timApp.document.viewcontext import ViewRoute, ViewContext
from timApp.user.user import Consent
from tim_common.marshmallow_dataclass import class_schema
from tim_common.utils import DurationSchema


class EmptyWarning:
    def warn(self, a: Any, b: Any) -> None:
        pass


# We don't want a runtime DeprecationWarning about verify_json_params.
# It's enough that PyCharm marks it as deprecated.
# noinspection PyRedeclaration
warnings = EmptyWarning()  # type: ignore[assignment]


def verify_json_params(
    *args: str,
    require: bool = True,
    default: Any = None,
    error_msgs: list[str] | None = None,
) -> list[Any]:
    """Gets the specified JSON parameters from the request.

    :param default: The default value for the parameter if it is not found from the request.
    :param require: If True and the parameter is not found, the request is aborted.
    """
    warnings.warn(
        'Do not use this function in new code. Define a dataclass and use "use_model" decorator in route.',
        DeprecationWarning,
    )
    result = []
    json_params = request.get_json(silent=True) or {}
    if error_msgs is not None:
        assert len(args) == len(error_msgs)
    for arg, err in zip(args, error_msgs or itertools.repeat(None, len(args))):
        if arg in json_params:
            val = json_params[arg]
        elif not require:
            val = default
        else:
            raise RouteException(err or f"Missing required parameter in request: {arg}")
            return []

        result.append(val)
    return result


def get_option(
    req: Request | ViewContext,
    name: str,
    default: Any,
    cast: type | None = None,
) -> Any:
    if name not in req.args:
        return default
    result = req.args[name]
    lresult = result.lower()
    if isinstance(default, bool) or cast is bool:
        if len(lresult) == 0:
            return default
        if "f0".find(lresult[0]) >= 0:
            return False
        if "t1".find(lresult[0]) >= 0:
            return True
        return True
    if isinstance(default, int) or cast is int:
        try:
            return int(lresult)
        except ValueError:
            return default
    if cast is not None:
        try:
            result = cast(result)
        except ValueError:
            return default
    return result


def is_testing() -> bool:
    return current_app.config["TESTING"]


def is_localhost() -> bool:
    return current_app.config["TIM_HOST"] in ("http://localhost", "http://caddy")


def get_active_host_url() -> str:
    """
    Returns the URL of the currently active host URL.
    If the call is made while handling a request, the host that was used in the request is returned.
    Otherwise, the host URL is read from the configuration.

    :return: Active host URL
    """
    if has_request_context():
        return request.host_url
    return f"{current_app.config['TIM_HOST']}/"


def get_consent_opt() -> Consent | None:
    consent_opt = get_option(request, "consent", "any")
    if consent_opt == "true":
        consent: Consent | None = Consent.CookieAndData
    elif consent_opt == "false":
        consent = Consent.CookieOnly
    elif consent_opt == "any":
        consent = None
    else:
        raise RouteException(
            'Invalid consent option. Must be "true", "false" or "any".'
        )
    return consent


def get_request_time() -> str | None:
    try:
        return f"{time.monotonic() - g.request_start_time:.3g}s"
    except AttributeError:
        return None


def get_request_message(
    status_code: int | None = None,
    include_body: bool = False,
    include_time: bool = True,
    is_before: bool = False,
) -> str:
    name = get_current_user_name()
    session_id = get_current_session_id()
    if session_id:
        session_id = f" ({session_id})"
    else:
        session_id = ""
    if current_app.config["LOG_HOST"]:
        url_or_path = request.url
    else:
        url_or_path = request.full_path if request.query_string else request.path
    ua: UserAgent = request.user_agent
    msg = f"""
{name}{session_id}
[{request.remote_addr}]:
{request.method}
{url_or_path}
{status_code or ""}
{get_request_time() if include_time else ""}
{ua.platform}/{ua.browser}/{ua.version}
{os.getpid()}
{"BEFORE" if is_before else ""}
""".replace(
        "\n", " "
    ).strip()
    if not include_body or request.method not in ("POST", "PUT", "DELETE"):
        return msg
    return f"{msg}\n\n{pprint.pformat(request.get_json(silent=True) or request.get_data(as_text=True))}"


class RouteException(HTTPException):
    code = 400


class NotExist(HTTPException):
    code = 404


@dataclass
class JSONException(Exception):
    description: str
    code: int = 400


def load_data_from_req(schema: type[Schema]) -> Any:
    ps = schema()
    try:
        j = request.get_json()
        if j is None:
            raise JSONException(description="JSON payload missing.")
        p = ps.load(j)
    except ValidationError as e:
        raise JSONException(description=json.dumps(e.messages, sort_keys=True))
    return p


ModelType = TypeVar("ModelType")


def use_model(
    m: type[ModelType],
) -> Callable[[Callable[[ModelType], Response]], Callable[[ModelType], Response]]:
    if not is_dataclass(m):
        raise Exception("use_model requires a dataclass")
    return use_args(class_schema(m, base_schema=DurationSchema)())


never_urlmacros = {"unlock", "nocache"}


def get_urlmacros_from_request() -> tuple[tuple[str, str], ...]:
    return get_urlmacros_from_dict(request.args)


def get_urlmacros_from_dict(d: dict[str, str]) -> tuple[tuple[str, str], ...]:
    urlmacros = tuple(
        (key, val) for key, val in d.items() if key not in never_urlmacros
    )
    return urlmacros


def get_urlmacros_from_request_dict() -> dict[str, str]:
    return {key: val for key, val in request.args.items() if key not in never_urlmacros}


def view_ctx_with_urlmacros(
    route: ViewRoute,
    hide_names_requested: bool = False,
    urlmacros: dict[str, str] | None = None,
    **kwargs: Any,
) -> ViewContext:
    return ViewContext(
        route,
        False,
        hide_names_requested=hide_names_requested,
        urlmacros=get_urlmacros_from_dict(urlmacros)
        if urlmacros is not None
        else get_urlmacros_from_request(),
        **kwargs,
    )


def get_from_url(url: str) -> str:
    parsed = urlparse(url)
    if not parsed.netloc and not parsed.scheme:
        host = f"http://caddy" if is_localhost() else current_app.config["TIM_HOST"]
        url = host + url
    try:
        r = requests.get(url)
    except Exception as ex:
        raise RouteException(str(ex) + " " + url)
    return r.content.decode("utf-8")
