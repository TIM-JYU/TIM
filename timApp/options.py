# -*- coding: utf-8 -*-


def get_option(request, name, default):
    if name not in request.args: return default
    result = request.args[name]
    lresult = result.lower()
    if isinstance(default, bool):
        if lresult == "false":
            return False
        if lresult == "true":
            return True
    return result
