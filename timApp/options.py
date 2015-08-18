# -*- coding: utf-8 -*-


def get_option(request, name, default):
    if name not in request.args: return default
    result = request.args[name]
    lresult = result.lower()
    if type(default) is type(True):
        if lresult == "false": return False
        if lresult == "true": return True
    return result
