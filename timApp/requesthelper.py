from typing import List

from flask import Request
from flask import request, abort

from timApp.timdb.invalidreferenceexception import InvalidReferenceException


def verify_json_params(*args: str, require=True, default=None):
    """Gets the specified JSON parameters from the request.

    :param default: The default value for the parameter if it is not found from the request.
    :param require: If True and the parameter is not found, the request is aborted.
    """
    result = ()
    json_params = request.get_json() or {}
    for arg in args:
        if arg in json_params:
            val = json_params[arg]
        elif not require:
            val = default
        else:
            abort(400, 'Missing required parameter in request: {}'.format(arg))
            return ()

        result += (val,)
    return result


def unpack_args(*args, types):
    result = ()
    json_params = request.args
    for idx, arg in enumerate(args):
        if arg not in json_params:
            abort(400, 'Missing required parameter in request: {}'.format(arg))
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


def get_option(req: Request, name: str, default, cast=None):
    if name not in req.args:
        return default
    result = req.args[name]
    lresult = result.lower()
    if isinstance(default, bool):
        if lresult == "false":
            return False
        if lresult == "true":
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
