import itertools
import json
import pprint
import time
import warnings
from typing import Optional, Type, TypeVar, Callable

from dataclasses import is_dataclass
from flask import Request, current_app, g, Response
from flask import request, abort
from marshmallow import ValidationError, Schema
from webargs.flaskparser import use_args
from werkzeug.exceptions import HTTPException
from werkzeug.wrappers import BaseRequest

from marshmallow_dataclass import class_schema
from timApp.auth.sessioninfo import get_current_user_object
from timApp.timdb.exceptions import InvalidReferenceException
from timApp.user.user import Consent


class EmptyWarning:
    def warn(self, a, b):
        pass


# We don't want a runtime DeprecationWarning about verify_json_params.
# It's enough that PyCharm marks it as deprecated.
# noinspection PyRedeclaration
warnings = EmptyWarning()


def verify_json_params(*args: str, require=True, default=None, error_msgs=None):
    """Gets the specified JSON parameters from the request.

    :param default: The default value for the parameter if it is not found from the request.
    :param require: If True and the parameter is not found, the request is aborted.
    """
    warnings.warn('Do not use this function in new code. Define a dataclass and use "use_model" decorator in route.', DeprecationWarning)
    result = ()
    json_params = request.get_json() or {}
    if error_msgs is not None:
        assert len(args) == len(error_msgs)
    for arg, err in zip(args, error_msgs or itertools.repeat(None, len(args))):
        if arg in json_params:
            val = json_params[arg]
        elif not require:
            val = default
        else:
            abort(400, err or f'Missing required parameter in request: {arg}')
            return ()

        result += (val,)
    return result


def get_referenced_pars_from_req(par):
    if par.is_reference() and not par.is_translation():
        try:
            return [ref_par for ref_par in par.get_referenced_pars(set_html=False)]
        except InvalidReferenceException as e:
            abort(404, str(e))
    else:
        return [par]


def get_option(req: Request, name: str, default, cast=None):
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


def is_xhr(req: BaseRequest):
    """Same as req.is_xhr but without the deprecation warning."""
    return req.environ.get(
        'HTTP_X_REQUESTED_WITH', ''
    ).lower() == 'xmlhttprequest'


def is_testing():
    return current_app.config['TESTING']


def is_localhost():
    return current_app.config['TIM_HOST'] in ('http://localhost', 'http://nginx')


def get_consent_opt() -> Optional[Consent]:
    consent_opt = get_option(request, 'consent', 'any')
    if consent_opt == 'true':
        consent = Consent.CookieAndData
    elif consent_opt == 'false':
        consent = Consent.CookieOnly
    elif consent_opt == 'any':
        consent = None
    else:
        return abort(400, 'Invalid consent option. Must be "true", "false" or "any".')
    return consent


def get_request_time():
    try:
        return f'{time.monotonic() - g.request_start_time:.3g}s'
    except AttributeError:
        return None


def get_request_message(status_code=None, include_body=False):
    msg = f"""
{get_current_user_object().name}
[{request.headers.get("X-Forwarded-For") or request.remote_addr}]:
{request.method}
{request.full_path if request.query_string else request.path}
{status_code or ""}
{get_request_time()}""".replace('\n', ' ').strip()
    if not include_body or request.method not in ('POST', 'PUT', 'DELETE'):
        return msg
    return f'{msg}\n\n{pprint.pformat(request.get_json(silent=True) or request.get_data(as_text=True))}'


class JSONException(HTTPException):
    code = 400


def load_data_from_req(schema: Type[Schema]):
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

def use_model(m: Type[ModelType]) -> Callable[[Callable[[ModelType], Response]], None]:
    if not is_dataclass(m):
        raise Exception('use_model requires a dataclass')
    return use_args(class_schema(m)())
