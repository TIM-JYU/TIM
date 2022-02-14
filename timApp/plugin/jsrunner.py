from dataclasses import dataclass
from typing import Any

import requests

from timApp.plugin.containerLink import get_plugin


@dataclass
class JsRunnerParams:
    code: str
    data: Any
    error_text: str = ""
    caller: str = ""


class JsRunnerError(Exception):
    pass


def jsrunner_run(params: JsRunnerParams) -> tuple[Any, str]:
    """
    Run JavaScript code in jsrunner.
    """
    runurl = get_plugin("jsrunner").host + "runScript/"
    r = requests.request(
        "post", runurl, json={"code": params.code, "data": params.data}
    )
    result = r.json()
    error = result.get("error")
    if error:
        if params.error_text and error.find("Script failed to return") >= 0:
            error += "\n" + params.error_text
        if params.caller:
            error = params.caller + "\n" + error
        raise JsRunnerError(error)
    data = result.get("result", [])
    output = result.get("output", "")
    return data, output
