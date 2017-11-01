import itertools

from flask import Request
from flask import request, abort

from timApp.timdb.invalidreferenceexception import InvalidReferenceException


def verify_json_params(*args: str, require=True, default=None, error_msgs=None):
    """Gets the specified JSON parameters from the request.

    :param default: The default value for the parameter if it is not found from the request.
    :param require: If True and the parameter is not found, the request is aborted.
    """
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


def unpack_args(*args, types):
    result = ()
    json_params = request.args
    for idx, arg in enumerate(args):
        if arg not in json_params:
            abort(400, f'Missing required parameter in request: {arg}')
        result += types[idx](json_params[arg]),
    return result


def get_referenced_pars_from_req(par):
    if par.is_reference():
        try:
            return [ref_par for ref_par in par.get_referenced_pars(set_html=False, tr_get_one=False)]
        except InvalidReferenceException as e:
            abort(404, str(e))
    else:
        return [par]


def get_boolean(s, default, cast=None):
    if s is None:
        return default
    if isinstance(s, bool):
        return s
    if isinstance(s, int):
        return s != 0
    result = s
    lresult = result.lower()
    if isinstance(default, bool):
        if len(lresult) == 0:
            return default
        if "f0y".find(lresult[0]) >= 0:
            return False
        if "t1n".find(lresult[0]) >= 0:
            return True
        return True
    if isinstance(default, int):
        try:
            return int(lresult)
        except ValueError:
            return default
    if cast is not None:
        try:
            result = cast(result)
        except ValueError:
            pass
    return result


def get_option(req: Request, name: str, default, cast=None):
    if name not in req.args:
        return default
    result = req.args[name]
    lresult = result.lower()
    if isinstance(default, bool):
        if len(lresult) == 0:
            return default
        if "f0".find(lresult[0]) >= 0:
            return False
        if "t1".find(lresult[0]) >= 0:
            return True
        return True
    if isinstance(default, int):
        try:
            return int(lresult)
        except ValueError:
            return default
    if cast is not None:
        try:
            result = cast(result)
        except ValueError:
            pass
    return result
