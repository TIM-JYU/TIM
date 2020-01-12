import json
import requests

from flask import abort
from timApp.plugin.containerLink import get_plugin

def jsrunner_run(params):
    """
    Run code by jsrunner
    params = {'code': 'return data*data;', 'data': 2}
    return data, output
    """
    runurl = get_plugin('jsrunner').get("host") + 'runScript/'
    r = requests.request('post', runurl, data=params)
    result = json.loads(r.text)
    error = result.get('error', '')
    if error:
        abort(400, error)
    data = result.get("result", [])
    output = result.get("output", "")
    return data, output
