from dataclasses import dataclass
from typing import Any

import requests

from timApp.plugin.containerLink import get_plugin


@dataclass
class JsRunnerParams:
    code: str
    data: Any


class JsRunnerError(Exception):
    pass


def jsrunner_run(params: JsRunnerParams):
    """
    Run JavaScript code in jsrunner.
    """
    runurl = get_plugin('jsrunner').get("host") + 'runScript/'
    r = requests.request('post', runurl, json={'code': params.code, 'data': params.data})
    result = r.json()
    error = result.get('error')
    if error:
        raise JsRunnerError(error)
    data = result.get("result", [])
    output = result.get("output", "")
    return data, output
