import itertools
import json
import os
import pprint
import time
import warnings
from dataclasses import is_dataclass, dataclass
from typing import Optional, Type, TypeVar, Callable, Any, List, Tuple, Union

from flask import Request, current_app, g, Response
from flask import request
from marshmallow import ValidationError, Schema
from webargs.flaskparser import use_args
from werkzeug.exceptions import HTTPException
from werkzeug.wrappers import BaseRequest

from timApp.auth.sessioninfo import get_current_user_name
from timApp.document.docparagraph import DocParagraph
from timApp.document.viewcontext import ViewRoute, ViewContext
from tim_common.marshmallow_dataclass import class_schema
from tim_common.utils import DurationSchema
from timApp.timdb.exceptions import InvalidReferenceException
from timApp.user.user import Consent


class EmptyWarning:
    def warn(self, a: Any, b: Any) -> None:
        pass


# We don't want a runtime DeprecationWarning about verify_json_params.
# It's enough that PyCharm marks it as deprecated.
# noinspection PyRedeclaration
warnings = EmptyWarning()  # type: ignore[assignment]


def verify_json_params(*args: str, require: bool=True, default: Any=None, error_msgs: Optional[List[str]]=None) -> List[Any]:
    """Gets the specified JSON parameters from the request.

    :param default: The default value for the parameter if it is not found from the request.
    :param require: If True and the parameter is not found, the request is aborted.
    """
    warnings.warn('Do not use this function in new code. Define a dataclass and use "use_model" decorator in route.', DeprecationWarning)
    result = []
    json_params = request.get_json() or {}
    if error_msgs is not None:
        assert len(args) == len(error_msgs)
    for arg, err in zip(args, error_msgs or itertools.repeat(None, len(args))):
        if arg in json_params:
            val = json_params[arg]
        elif not require:
            val = default
        else:
            raise RouteException(err or f'Missing required parameter in request: {arg}')
            return []

        result.append(val)
    return result


def get_option(req: Union[Request, ViewContext], name: str, default: Any, cast: Optional[Type]=None) -> Any:
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


def is_xhr(req: BaseRequest) -> bool:
    """Same as req.is_xhr but without the deprecation warning."""
    return req.environ.get(
        'HTTP_X_REQUESTED_WITH', ''
    ).lower() == 'xmlhttprequest'


def is_testing() -> bool:
    return current_app.config['TESTING']


def is_localhost() -> bool:
    return current_app.config['TIM_HOST'] in ('http://localhost', 'http://caddy')


def get_consent_opt() -> Optional[Consent]:
    consent_opt = get_option(request, 'consent', 'any')
    if consent_opt == 'true':
        consent: Optional[Consent] = Consent.CookieAndData
    elif consent_opt == 'false':
        consent = Consent.CookieOnly
    elif consent_opt == 'any':
        consent = None
    else:
        raise RouteException('Invalid consent option. Must be "true", "false" or "any".')
    return consent


def get_request_time() -> Optional[str]:
    try:
        return f'{time.monotonic() - g.request_start_time:.3g}s'
    except AttributeError:
        return None


@dataclass
class UA:
    platform: str
    browser: str
    version: str


def get_request_message(status_code: Optional[int]=None, include_body: bool=False) -> str:
    name = get_current_user_name()
    if current_app.config['LOG_HOST']:
        url_or_path = request.url
    else:
        url_or_path = request.full_path if request.query_string else request.path
    ua: UA = request.user_agent  # type: ignore
    msg = f"""
{name}
[{request.remote_addr}]:
{request.method}
{url_or_path}
{status_code or ""}
{get_request_time()}
{ua.platform}/{ua.browser}/{ua.version}
{os.getpid()}
""".replace('\n', ' ').strip()
    if not include_body or request.method not in ('POST', 'PUT', 'DELETE'):
        return msg
    return f'{msg}\n\n{pprint.pformat(request.get_json(silent=True) or request.get_data(as_text=True))}'


class RouteException(HTTPException):
    code = 400


class NotExist(HTTPException):
    code = 404


@dataclass
class JSONException(Exception):
    description: str
    code: int = 400


def load_data_from_req(schema: Type[Schema]) -> Any:
    ps = schema()
    try:
        j = request.get_json()
        if j is None:
            raise JSONException(description='JSON payload missing.')
        p = ps.load(j)
    except ValidationError as e:
        raise JSONException(description=json.dumps(e.messages, sort_keys=True))
    return p

ModelType = TypeVar('ModelType')

def use_model(m: Type[ModelType]) -> Callable[[Callable[[ModelType], Response]], Callable[[ModelType], Response]]:
    if not is_dataclass(m):
        raise Exception('use_model requires a dataclass')
    return use_args(class_schema(m, base_schema=DurationSchema)())


never_urlmacros = {'unlock', 'nocache'}


def get_urlmacros_from_request() -> Tuple[Tuple[str, str], ...]:
    urlmacros = tuple((key, val) for key, val in request.args.items() if key not in never_urlmacros)
    return urlmacros


def view_ctx_with_urlmacros(route: ViewRoute, hide_names_requested: bool = False) -> ViewContext:
    return ViewContext(
        route,
        False,
        hide_names_requested=hide_names_requested,
        urlmacros=get_urlmacros_from_request(),
    )
